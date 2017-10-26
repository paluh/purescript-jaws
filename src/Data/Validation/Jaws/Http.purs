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
import Data.Validation.Jaws.Validation (Validation(..), pureV, tag)
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
optional v = dimap (head >>> note unit) hush (right v)

catMaybesV :: forall a e m. (Monad m) ⇒ Validation m e (Array (Maybe a)) (Array a)
catMaybesV = pureV (catMaybes >>> Right)

emptyArrayV ∷ ∀ a m. (Monad m) ⇒ Validation m (Array a) (Array a) Unit
emptyArrayV = pureV $ (case _ of
  [] → Right unit
  a → Left a)

nonEmptyArray ∷ ∀ a m. (Monad m) ⇒ Validation m Unit (Array a) (NonEmpty Array a)
nonEmptyArray  =
  pureV $ (uncons >=> (\r → pure (r.head :| r.tail))) >>> note unit

nonEmptyArray' ∷ ∀ a v m. (Monad m) ⇒ Validation m (Variant (nonEmptyArray ∷ Unit | v)) (Array a) (NonEmpty Array a)
nonEmptyArray' = tag (SProxy ∷ SProxy "nonEmptyArray") nonEmptyArray

scalar ∷ ∀ a m. (Monad m) ⇒ Validation m (Array a) (Array a) a
scalar = pureV s
 where
  s [a] = Right a
  s arr = Left arr

scalar' ∷ ∀ a v m. (Monad m) ⇒ Validation m (Variant (scalar ∷ Array a | v)) (Array a) a
scalar' = tag (SProxy ∷ SProxy "scalar") scalar

int ∷ ∀ m. (Monad m) ⇒ Validation m String String Int
int = pureV (\s → note s (fromString s))

int' ∷ ∀ m v. (Monad m) ⇒ Validation m (Variant (int ∷ String | v)) String Int
int' = tag (SProxy ∷ SProxy "int") int

nonEmptyString ∷ ∀ m v. (Monad m) ⇒ Validation m (Variant (nonEmptyArray ∷ Unit, scalar ∷ Array String | v)) (Array (Maybe String)) String
nonEmptyString =
  catMaybesV >>> scalar' >>> (tag (SProxy ∷ SProxy "nonEmptyArray") $ pureV (case _ of
    "" → Left unit
    s → Right s))
