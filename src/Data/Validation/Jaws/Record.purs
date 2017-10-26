module Data.Validation.Jaws.Record where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Record (insert)
import Data.Record.Builder (build, merge)
import Data.Symbol (SProxy)
import Data.Validation.Jaws.Validation (Validation(..), runValidation, tag)
import Data.Variant (Variant)
import Type.Prelude (class IsSymbol, class RowLacks)

-- | This is just (tok → a → m b)
newtype Builder m tok a b = Builder (ReaderT {tok ∷ tok, a ∷ a} m b)
derive instance functorBuilder ∷ (Functor m) ⇒ Functor (Builder m e a)
derive newtype instance applyBuilder ∷ (Monad m) ⇒ Apply (Builder m e a)
derive newtype instance applicativeBuilder ∷ (Monad m) ⇒ Applicative (Builder m e a)
derive newtype instance bindBuilder ∷ (Monad m) ⇒ Bind (Builder m e a)
derive newtype instance monadBuilder ∷ (Monad m) ⇒ Monad (Builder m e a)

-- What about those - can I derive them somehow too?
instance categoryBuilder ∷ (Monad m) ⇒ Category (Builder m tok) where
id = Builder (ReaderT (\{a} → pure a))

instance semigroupoidBuilder ∷ (Bind m) ⇒ Semigroupoid (Builder m tok) where
  compose (Builder b1) (Builder b2) =
    Builder (ReaderT $ \r@{tok} → runReaderT b2 r >>= (\b → runReaderT b1 {tok, a:b}))

instance profunctorBuilder ∷ (Monad m) ⇒ Profunctor (Builder m tok) where
  dimap f g (Builder v) =
    Builder (ReaderT $ \r@{tok, a} → runReaderT v {tok, a: f a} >>= \b → pure (g b))

instance choiceBuilder ∷ (Monad m) ⇒ Choice (Builder m tok) where
  left (Builder v) =
    Builder (ReaderT v')
   where
    v' {tok, a: Left input} = Left <$> (runReaderT v {tok, a: input})
    v' {a: (Right r)} = pure (Right r)
  right (Builder v) =
    Builder (ReaderT v')
   where
    v' {tok, a: (Right input)} = Right <$> (runReaderT v {tok, a: input})
    v' {a: Left l} = pure (Left l)

data Result i v =
  Invalid i | Valid i v

valid ∷ ∀ i v. v → Result (Either i v) v
valid v = Valid (Right v) v

instance functorResult ∷ Functor (Result i) where
  map f (Invalid i) = Invalid i
  map f (Valid i v) = Valid i (f v)

mapI ∷ ∀ i i' v. (i → i') → Result i v → Result i' v
mapI f (Invalid i) = Invalid (f i)
mapI f (Valid i v) = Valid (f i) v

instance bifunctorResult ∷ Bifunctor Result where
  bimap g f r = map f (mapI g r)

-- | Aggregate validation results.
-- |
-- | To simplify final result extraction
-- | it passes two types of result:
-- |
-- | * just `unwraped` resulting value
-- |
-- | * valid value wrapped in `Right` so it can
-- |   be easily turned into invalid value
-- |
-- | i, i' - invalid result types
-- | v, v' - valid result types
type RecordBuilder m tok i i' v v' =
  Builder m tok (Result (Record i) (Record v)) (Result (Record i') (Record v'))

buildRecord ∷ ∀ i m tok v. (Functor m) ⇒ RecordBuilder m tok () i () v → Validation m (Record i) tok (Record v)
buildRecord (Builder f) =
  Validation (ReaderT $ (\tok → ExceptT $ toEither <$> (runReaderT f {tok, a: Valid {} {}})))
 where
  toEither = case _  of
    Invalid i → Left i
    Valid i v → Right v

buildRecord' :: forall e e' i l m t v
  . RowCons l (Record i) e e'
  => Monad m
  => IsSymbol l
  => SProxy l
  -> Builder m t (Result {} {}) (Result (Record i) (Record v))
  -> Validation m (Variant e') t (Record v)
buildRecord' l b = tag l (buildRecord b)

addField ∷ ∀ a b e i i' l m v v'
  . (IsSymbol l)
  ⇒ (RowCons l (Either e b) i i')
  ⇒ (RowCons l b v v')
  ⇒ (RowLacks l i)
  ⇒ (RowLacks l v)
  ⇒ (Functor m)
  ⇒ SProxy l
  → Validation m e a b
  → RecordBuilder m a i i' v v'
addField p v =
  Builder (ReaderT (\{tok, a} → toRecord a <$> runValidation v tok))
 where
  toRecord (Valid ir vr) r@(Right b) = Valid (insert p r ir) (insert p b vr)
  toRecord (Valid ir _) e = Invalid (insert p e ir)
  toRecord (Invalid ir) e = Invalid (insert p e ir)

combine :: forall i i1 i2 i3 m tok v v1 v2 v3
  . Union i1 i2 i3
  ⇒ Union v1 v2 v3
  ⇒ (Apply m)
  ⇒ RecordBuilder m tok i i1 v v1
  → RecordBuilder m tok i i2 v v2
  → RecordBuilder m tok i i3 v v3
combine (Builder v1) (Builder v2) =
  Builder (ReaderT $ \r -> combine' <$> (runReaderT v1 r) <*> (runReaderT v2 r))
 where
  combine' r1 r2 = case r1, r2 of
    Invalid i1, Invalid i2 → Invalid (build (merge i2) i1)
    Invalid i1, Valid i2 _ → Invalid (build (merge i2) i1)
    Valid i1 _, Invalid i2 → Invalid (build (merge i2) i1)
    Valid i1 u1, Valid i2 u2 → Valid (build (merge i2) i1) (build (merge u2) u1)
