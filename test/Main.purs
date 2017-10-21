module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.StrMap (empty, fromFoldable)
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Data.Validation.Jaws (Query, TrackedValidation, int, nonEmptyStr, optional', queryField, run)
import Data.Variant (SProxy(..), Variant, inj)
import Debug.Trace (traceAnyA)



greaterThan ∷ ∀ v. Int → Int → Either (Variant (toSmall ∷ Unit | v)) Int
greaterThan min i
  | min < i = Right i
  | otherwise = Left (inj (SProxy ∷ SProxy "toSmall") unit)

emailErr = inj (SProxy ∷ SProxy "emailFormat") unit

email ∷ ∀ v. String → Either (Variant (emailFormat ∷ Unit | v)) Email
email s | contains (Pattern "@") s = Right (Email s)
        | otherwise = Left emailErr

newtype Email = Email String
derive instance newtypeEmail ∷ Newtype Email _
derive newtype instance showShow ∷ Show Email

newtype Nickname = Nickname String
derive instance newtypeNickname ∷ Newtype Nickname _
derive newtype instance showNickname ∷ Show Nickname

newtype GoodPerson = GoodPerson
  { nickname ∷ Nickname
  , age ∷ Maybe Int
  , email ∷ Email
  }
derive instance genericGoodPerson ∷ Generic GoodPerson _
instance showGoodPerson ∷ Show GoodPerson where
  show = genericShow

personForm ∷
  ∀ vr ir.
  TrackedValidation Query () (nickname ∷ _, age ∷ _, email ∷ _) () (nickname ∷ _, age ∷ _, email ∷ _)
personForm =
  queryField (SProxy ∷ SProxy "nickname") (nonEmptyStr >=> (pure <<< Nickname)) >>>
  queryField (SProxy ∷ SProxy "age") (optional' (int >=> greaterThan 18)) >>>
  queryField (SProxy ∷ SProxy "email") (nonEmptyStr >=> email)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let
    validateAndPrint q =
      case run personForm q of
        Right v → logShow (GoodPerson v)
        Left e → traceAnyA e
  validateAndPrint empty
  validateAndPrint (fromFoldable [Tuple "nickname" [Just "paluh"], Tuple "age" [Just "666"], Tuple "email" [Just "paluho@gmail.com"]])
  validateAndPrint (fromFoldable [Tuple "nickname" [Just "paluh"], Tuple "age" [], Tuple "email" [Just "paluho@gmail.com"]])
  pure unit
