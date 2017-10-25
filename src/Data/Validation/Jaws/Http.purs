module Data.Validation.Jaws.Http where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Array (catMaybes, head, uncons)
import Data.Either (Either(..), hush, note)
import Data.Int (fromString)
import Data.Maybe (Maybe, fromMaybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (right)
import Data.StrMap (StrMap, lookup)
import Data.Validation.Jaws.Record (RecordBuilder, addField)
import Data.Validation.Jaws.Validation (Validation(..), PureValidation, pureV, tag)
import Data.Variant (Variant)
import Type.Prelude (class IsSymbol, class RowLacks, SProxy(..), reflectSymbol)

-- | This is just a playground for a HTTP validation

-- | In http query every key can be repeated
-- | and can have form:
-- | ?key ~ (Nothing) or ?key= ~ (Just "") or ?key=v1 ~ (Just "v1")
type QueryField = Array (Maybe String)
type Query = StrMap (Array (Maybe String))

addFieldFromQuery ∷ ∀ a e m s ir ir' vr vr'
  . (IsSymbol s)
  ⇒ (Functor m)
  ⇒ RowLacks s ir
  ⇒ RowCons s (Either e a) ir ir'
  ⇒ RowLacks s vr
  ⇒ RowCons s a vr vr'
  ⇒ SProxy s
  → Validation m e QueryField a
  → RecordBuilder m Query ir ir' vr vr'
addFieldFromQuery p (Validation v) =
  addField p (Validation (ReaderT (\query → runReaderT v (fromMaybe [] (lookup (reflectSymbol p) query)))))

optional ∷ ∀ a b e m. (Monad m) ⇒ Validation m e a b → Validation m e (Array a) (Maybe b)
optional v =
  dimap (head >>> note unit) hush (right v)

catMaybesV :: forall a e m. (Monad m) ⇒ Validation m e (Array (Maybe a)) (Array a)
catMaybesV = pureV (catMaybes >>> Right)

nonEmpty ∷ ∀ a m. (Monad m) ⇒ Validation m Unit (Array a) (NonEmpty Array a)
nonEmpty  =
  pureV $ (uncons >=> (\r → pure (r.head :| r.tail))) >>> note unit

nonEmpty' ∷ ∀ a v m. (Monad m) ⇒ Validation m (Variant (nonEmpty ∷ Unit | v)) (Array a) (NonEmpty Array a)
nonEmpty' = tag (SProxy ∷ SProxy "nonEmpty") nonEmpty

scalar ∷ ∀ a m. (Monad m) ⇒ Validation m (NonEmpty Array a) (NonEmpty Array a) a
scalar = pureV s
 where
  s (a :| []) = Right a
  s l = Left l

scalar' ∷ ∀ a v m. (Monad m) ⇒ Validation m (Variant (scalar ∷ NonEmpty Array a | v)) (NonEmpty Array a) a
scalar' = tag (SProxy ∷ SProxy "scalar") scalar

int :: PureValidation String String Int
int = pureV (\s → note s (fromString s))

int' ∷ PureValidation (Variant (int ∷ String)) String Int
int' = tag (SProxy ∷ SProxy "int") int

nonEmptyString ∷ ∀ m v. (Monad m) ⇒ Validation m (Variant (nonEmpty ∷ Unit, scalar ∷ NonEmpty Array String | v)) (Array (Maybe String)) String
nonEmptyString =
  catMaybesV >>> nonEmpty' >>> scalar' >>> (tag (SProxy ∷ SProxy "nonEmpty") $ pureV (case _ of
    "" → Left unit
    s → Right s))
