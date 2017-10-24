module Data.Validation.Jaws where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Trans (ExceptT(..))
import Data.Array (catMaybes, head, uncons)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(Left, Right), hush, note)
import Data.EitherR (fmapLT)
import Data.Int (fromString)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice, right)
import Data.Record (get, insert)
import Data.Record.Builder (build, merge)
import Data.StrMap (StrMap, lookup)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Variant (Variant, inj)
import Type.Row (class RowLacks)

-- | Don't stare to long at this type it's really general...
-- | Check `Validation` and `ValidationToRecord`
newtype ValidationM m tok a b = ValidationM (tok → a → m b)

instance semigroupoidValidationM ∷ (Bind m) ⇒ Semigroupoid (ValidationM m tok) where
  compose (ValidationM v2) (ValidationM v1) =
    ValidationM (\tok → v1 tok >=> v2 tok)

instance categoryValidation ∷ (Monad m) ⇒ Category (ValidationM m tok) where
  id = ValidationM (const pure)

instance profunctorValidation ∷ (Monad m) ⇒ Profunctor (ValidationM m tok) where
  dimap f g (ValidationM v) =
    ValidationM (\tok → (f >>> pure) >=> v tok >=> (g >>> pure))

instance choiceValidation ∷ (Monad m) ⇒ Choice (ValidationM m tok) where
  left (ValidationM v) =
    ValidationM v'
   where
    v' tok (Left input) = Left <$> (v tok input)
    v' tok (Right r) = pure (Right r)
  right (ValidationM v) =
    ValidationM v'
   where
    v' tok (Left l) = pure (Left l)
    v' tok (Right input) = Right <$> (v tok input)

runValidationM ∷ ∀ a b m tok. ValidationM m tok a b → tok → a → m b
runValidationM (ValidationM v) = v

type Validation m tok a e b = ValidationM (ExceptT e m) tok a b

-- | This is "pure" (in `Reader tok` sens) validation - isomorphic to:
-- | a → m (Either e b)
type PureValidation a e b = ∀ m tok. (Monad m) ⇒ Validation m tok a e b

tag :: forall a b e m p r r' tok
  . RowCons p e r r'
  ⇒ Monad m
  ⇒ IsSymbol p
  ⇒ SProxy p
  → Validation m tok a e b
  → Validation m tok a (Variant r') b
tag p (ValidationM v) =
  ValidationM (\tok result → fmapLT (inj p) (v tok result))


-- This is just (Either i (Tuple i v))
-- but this alias gave me nearly nothing
-- in this context so I made brand new type
data ResultWithFailover i v =
  Invalid i | Valid i v

instance functorResultWithFailover ∷ Functor (ResultWithFailover i) where
  map f (Invalid i) = Invalid i
  map f (Valid i v) = Valid i (f v)

mapI ∷ ∀ i i' v. (i → i') → ResultWithFailover i v → ResultWithFailover i' v
mapI f (Invalid i) = Invalid (f i)
mapI f (Valid i v) = Valid (f i) v

instance bifunctorResultWithFailover ∷ Bifunctor ResultWithFailover where
  bimap g f r = map f (mapI g r)


-- | Aggregate validation results.
-- |
-- | To simplify final result extraction
-- | it passes two types of result:
-- |
-- | * just `unwraped` resulting value
-- |
-- | * valid value wrapped in `Left` so it can
-- |   be easily turned into invalid value
-- |
-- | i, i' - invalid result types
-- | v, v' - valid result types
type ValidationWithFailover m tok i i' v v' =
  ValidationM
    m
    tok
    (ResultWithFailover i v)
    (ResultWithFailover i' v')

-- | XXX: migrate to Data.Record.Builder
type ValidationToRecord m tok i i' v v' =
  ValidationWithFailover m tok (Record i) (Record i') (Record v) (Record v')

toValidation ∷ ∀ i m tok v. (Functor m) ⇒ ValidationToRecord m tok () i () v → Validation m tok Unit (Record i) (Record v)
toValidation (ValidationM f) =
  ValidationM (\tok _ → ExceptT $ toEither <$> (f tok (Valid {} {})))
 where
  toEither = case _  of
    Invalid i → Left i
    Valid i v → Right v

tagValidationWithFailover :: forall i i' r r' m p tok v v'
  . RowCons p i' r r'
  ⇒ (Functor m)
  ⇒ IsSymbol p
  ⇒ SProxy p
  → ValidationWithFailover m tok i i' v v'
  → ValidationWithFailover m tok i (Variant r') v v'
tagValidationWithFailover p (ValidationM v) =
  ValidationM (\tok result → mapI (inj p) <$> (v tok result))

combine :: forall i i1 i2 i3 m tok v v1 v2 v3
  . Union i1 i2 i3
  ⇒ Union v1 v2 v3
  ⇒ (Apply m)
  ⇒ ValidationToRecord m tok i i1 v v1
  → ValidationToRecord m tok i i2 v v2
  → ValidationToRecord m tok i i3 v v3
combine (ValidationM v1) (ValidationM v2) =
  ValidationM (\tok a -> combine' <$> (v1 tok a) <*> (v2 tok a))
 where
  combine' r1 r2 = case r1, r2 of
    Invalid i1, Invalid i2 → Invalid (build (merge i2) i1)
    Invalid i1, Valid i2 _ → Invalid (build (merge i2) i1)
    Valid i1 _, Invalid i2 → Invalid (build (merge i2) i1)
    Valid i1 u1, Valid i2 u2 → Valid (build (merge i2) i1) (build (merge u2) u1)

toRecordField ∷ ∀ b e i i' m p tok v v'
  . (IsSymbol p)
  ⇒ (RowCons p (Either e b) i i')
  ⇒ (RowCons p b v v')
  ⇒ (RowLacks p i)
  ⇒ (RowLacks p v)
  ⇒ (Functor m)
  ⇒ SProxy p
  → Validation m tok Unit e b
  → ValidationToRecord m tok i i' v v'
toRecordField p (ValidationM f) =
  ValidationM (\tok r → asRecord r <$> runExceptT (f tok unit))
 where
  asRecord (Valid ir vr) r@(Right v) = Valid (insert p r ir) (insert p v vr)
  asRecord (Valid ir _) e = Invalid (insert p e ir)
  asRecord (Invalid ir) e = Invalid (insert p e ir)

-- | In http query every key can be repeated
-- | and can have form:
-- | ?key ~ (Nothing) or ?key= ~ (Just "") or ?key=v1 ~ (Just "v1")
type QueryField = Array (Maybe String)
type Query = StrMap (Array (Maybe String))

fieldFromRecordField ∷ ∀ a b e ir ir' l m r r' vr vr'
  . (IsSymbol l)
  ⇒ (Functor m)
  ⇒ RowLacks l ir
  ⇒ RowCons l (Either e b) ir ir'
  ⇒ RowLacks l vr
  ⇒ RowCons l a r r'
  ⇒ RowCons l b vr vr'
  ⇒ SProxy l
  → Validation m a Unit e b
  → ValidationToRecord m (Record r') ir ir' vr vr'
fieldFromRecordField p (ValidationM v) =
  toRecordField p (ValidationM (\record _ → v (get p record) unit))

fieldFromRecord ∷ ∀ a b e ir ir' l m r r' vr vr'
  . (IsSymbol l)
  ⇒ (Functor m)
  ⇒ RowLacks l ir
  ⇒ RowCons l (Either e b) ir ir'
  ⇒ RowLacks l vr
  ⇒ RowCons l a r r'
  ⇒ RowCons l b vr vr'
  ⇒ SProxy l
  → Validation m (Record r') Unit e b
  → ValidationToRecord m (Record r') ir ir' vr vr'
fieldFromRecord p v =
  toRecordField p v

fieldFromQuery ∷ ∀ a e m s ir ir' vr vr'
  . (IsSymbol s)
  ⇒ (Functor m)
  ⇒ RowLacks s ir
  ⇒ RowCons s (Either e a) ir ir'
  ⇒ RowLacks s vr
  ⇒ RowCons s a vr vr'
  ⇒ SProxy s
  → Validation m QueryField Unit e a
  → ValidationToRecord m Query ir ir' vr vr'
fieldFromQuery p (ValidationM v) =
  toRecordField p (ValidationM (\query _ → v (fromMaybe [] (lookup (reflectSymbol p) query)) unit))

-- pureV2R f = ValidationM

-- lift function which takes token and result
pureV ∷ ∀ a b e m tok. (Monad m) ⇒ (tok → a → Either e b) → Validation m tok a e b
pureV f = ValidationM (\tok → f tok >>> pure >>> ExceptT)

-- lift function which cares only about already processed value
pureV' ∷ ∀ a b e. (a → Either e b) → PureValidation a e b
pureV' f = ValidationM (\_ → f >>> pure >>> ExceptT)

catMaybesV :: forall a e. PureValidation (Array (Maybe a)) e (Array a)
catMaybesV = pureV' (catMaybes >>> Right)

nonEmpty ∷ ∀ a. PureValidation (Array a) Unit (NonEmpty Array a)
nonEmpty  =
  pureV' $ \a → case uncons a of
    Nothing → Left unit
    Just { head, tail } → Right (head :| tail)

nonEmpty' ∷ ∀ a v. PureValidation (Array a) (Variant (nonEmpty ∷ Unit | v)) (NonEmpty Array a)
nonEmpty' = tag (SProxy ∷ SProxy "nonEmpty") nonEmpty

scalar ∷ ∀ a. PureValidation (NonEmpty Array a) Unit a
scalar = pureV' s
 where
  s (a :| []) = Right a
  s _ = Left unit

scalar' ∷ ∀ a v. PureValidation (NonEmpty Array a) (Variant (scalar ∷ Unit | v)) a
scalar' = tag (SProxy ∷ SProxy "scalar") scalar

optional ∷ ∀ a b e m tok. (Monad m) ⇒ Validation m tok a e b → Validation m tok (Array a) e (Maybe b)
optional v =
  dimap (head >>> note unit) hush (right v)

int :: PureValidation String Unit Int
int = pureV' (fromString >>> note unit)

int' ∷ PureValidation String (Variant (int ∷ Unit)) Int
int' = tag (SProxy ∷ SProxy "int") int

tokV ∷ ∀ a e i m tok. (Monad m) ⇒ Validation m tok Unit e tok
tokV = pureV $ (\tok _ → Right tok)

