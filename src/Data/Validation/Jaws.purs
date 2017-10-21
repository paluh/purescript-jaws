module Data.Validation.Jaws where

import Prelude

import Data.Either (Either, Either(..), isLeft)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Record (get, insert)
import Data.StrMap (StrMap, empty, fromFoldable, lookup)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Variant (inj, Variant)
import Type.Row (class RowLacks)

newtype Validation tok a b = Validation (tok → (a → b))

instance semigroupoidValidation ∷ Semigroupoid (Validation tok) where
  compose (Validation v1) (Validation v2) =
    Validation (\tok →
      let
        f1 = v1 tok
        f2 = v2 tok
      in (f1 <<< f2))

instance categoryValidation ∷ Category (Validation tok) where
  id = Validation (const id)

-- | Aggregate validation results.
-- |
-- | To simplify final result extraction
-- | it passes two types of result:
-- |
-- | * just `unwraped` resulting value
-- | * valid value wrapped in `Left` so it can
-- |   be easily turned into invalid value
-- |
-- | i, i' - invalid result types
-- | v, v' - valid result types
type TrackedValidation tok i i' v v' =
  Validation
    tok
    (Either (Record i) { v ∷ Record v, i ∷ Record i})
    (Either (Record i') { v ∷ Record v', i ∷ Record i'})

run ∷ ∀ i tok v. TrackedValidation tok () i () v → tok → Either (Record i) (Record v)
run (Validation f) tok =
  case f tok (Right {v: {}, i: {}}) of
    Right { v, i } → Right v
    Left i → Left i

-- | Adds validation step and inserts the result
-- | into final record
addValidationStep ∷ ∀ e a s ir ir' vr vr' tok
  . (IsSymbol s)
  ⇒ RowLacks s ir
  ⇒ RowCons s (Either e a) ir ir'
  ⇒ RowLacks s vr
  ⇒ RowCons s a vr vr'
  ⇒ SProxy s
  → (tok → Either e a)
  → TrackedValidation tok ir ir' vr vr'
addValidationStep p v = Validation (\tok result →
  case v tok, result of
    Left i, Left ir → Left (insert p (Left i) ir)
    Left i, Right {i: ir} → Left (insert p (Left i) ir)
    Right v, Left ir → Left (insert p (Right v) ir)
    Right v, Right {v: vr, i: ir} →
      Right
        { v: insert p v vr
        , i: insert p (Right v) ir })


-- | In http query every key can be repeated
-- | and can have form:
-- | ?key ~ (Nothing) or ?key= ~ (Just "") or ?key=v1 ~ (Just "v1")
type FieldValue = Array (Maybe String)
type Query = StrMap (Array (Maybe String))

queryField ∷ ∀ e a s ir ir' vr vr' tok
  . (IsSymbol s)
  ⇒ RowLacks s ir
  ⇒ RowCons s (Either e a) ir ir'
  ⇒ RowLacks s vr
  ⇒ RowCons s a vr vr'
  ⇒ SProxy s
  → (Array (Maybe String) → Either e a)
  → TrackedValidation Query ir ir' vr vr'
queryField p v =
  addValidationStep p (\query → v (fromMaybe [] (lookup (reflectSymbol p) query)))

missingErr ∷ ∀ v. Variant (missing ∷ Unit | v)
missingErr = inj (SProxy ∷ SProxy "missing") unit

emptyErr ∷ ∀ v. Variant (empty ∷ Unit | v)
emptyErr = inj (SProxy ∷ SProxy "empty") unit

repeatedErr ∷ ∀ v. Variant (repeated ∷ Unit | v)
repeatedErr = inj (SProxy ∷ SProxy "repeated") unit

present ∷ ∀ v. FieldValue → Either (Variant (missing ∷ Unit, repeated ∷ Unit | v)) (Maybe String)
present [] = Left missingErr
present [v] = Right v
present _ = Left repeatedErr

nonEmptyStr ∷ ∀ v. FieldValue → Either (Variant (missing ∷ Unit, repeated ∷ Unit, empty ∷ Unit | v)) String
nonEmptyStr = present >=> case _ of
    Nothing → Left emptyErr
    Just "" → Left emptyErr
    Just v → Right v

optional ∷ ∀ a v. (String → Either (Variant (repeated ∷ Unit | v)) a) → (FieldValue → Either (Variant (repeated ∷ Unit | v)) (Maybe a))
optional v i = do
  mr ← optional' i
  case mr of
    Nothing → pure Nothing
    Just r → Just <$> v r
 where
  optional' [] = Right Nothing
  optional' [Just s] = Right (Just s)
  optional' [Nothing] = Right Nothing
  optional' _ = Left repeatedErr


notNumberErr ∷ ∀ v. Variant (notNumber ∷ Unit | v)
notNumberErr = inj (SProxy ∷ SProxy "notNumber") unit

int ∷ ∀ v. String → Either (Variant (notNumber ∷ Unit | v)) Int
int s =
  case fromString s of
    Just i → Right i
    Nothing → Left notNumberErr

recordField ∷ ∀ e a s ir ir' vr vr' f r r' tok
  . (IsSymbol s)
  ⇒ RowLacks s ir
  ⇒ RowCons s (Either e a) ir ir'
  ⇒ RowLacks s vr
  ⇒ RowCons s a vr vr'
  ⇒ RowCons s f r r'
  ⇒ SProxy s
  → (f → Either e a)
  → TrackedValidation (Record r') ir ir' vr vr'
recordField p v =
  addValidationStep p (\record → v (get p record))

