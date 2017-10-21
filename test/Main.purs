module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Either (Either(..))
import Data.EitherR (fmapL)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.StrMap (empty, fromFoldable)
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Data.Validation.Jaws (Query, TrackedValidation, addValidationStep, int, nonEmptyStr, optional, queryField, run)
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
  TrackedValidation Query () (nickname ∷ _, age ∷ _, email ∷ _) () (nickname ∷ _, age ∷ _, email ∷ _)
personForm =
  queryField (SProxy ∷ SProxy "nickname") (nonEmptyStr >=> (Nickname >>> pure)) >>>
  queryField (SProxy ∷ SProxy "age") (optional (int >=> greaterThan 18)) >>>
  queryField (SProxy ∷ SProxy "email") (nonEmptyStr >=> email)


passwordFields ∷
  TrackedValidation Query () (password1 ∷ _, password2 ∷ _) () (password1 ∷ _, password2 ∷ _)
passwordFields =
  queryField (SProxy ∷ SProxy "password1") nonEmptyStr >>>
  queryField (SProxy ∷ SProxy "password2") nonEmptyStr

passwordsNotEqualErr = inj (SProxy ∷ SProxy "passwordNotEqual") unit

passwordsAreEqual p | p.password1 == p.password2 = Right p.password1
                    | otherwise = Left passwordsNotEqualErr

passwordForm ∷ _
passwordForm =
  (fmapL (inj (SProxy ∷ SProxy "fieldsErrors")) <$> run passwordFields) >=>
  (fmapL (inj (SProxy ∷ SProxy "formErrors")) <$> run form)
 where
  form = addValidationStep (SProxy ∷ SProxy "password") passwordsAreEqual

newtype Password = Password String
derive instance newtypePassword ∷ Newtype Password _
derive newtype instance showPassword ∷ Show Password


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let
    validateAndPrint f q = do
      case run f q of
        Right v → logShow (GoodPerson v)
        Left e → traceAnyA e
      log "\n"
    printResult r =
      case r of
        Right v → logShow (Password v.password)
        Left e → traceAnyA e

  validateAndPrint personForm empty
  validateAndPrint personForm (fromFoldable [Tuple "nickname" [Just "paluh"], Tuple "age" [Just "12"], Tuple "email" [Just "paluhogmail.com"]])
  validateAndPrint personForm (fromFoldable [Tuple "nickname" [Just "paluh"], Tuple "age" [Just "666"], Tuple "email" [Just "paluho@gmail.com"]])
  validateAndPrint personForm (fromFoldable [Tuple "nickname" [Just "paluh"], Tuple "age" [], Tuple "email" [Just "paluho@gmail.com"]])


  printResult $ passwordForm (fromFoldable [Tuple "password1" [Just "psss"], Tuple "password2" []])
  printResult $ passwordForm (fromFoldable [Tuple "password1" [Just "psss"], Tuple "password2" [Just "other"]])
  printResult $ passwordForm (fromFoldable [Tuple "password1" [Just "psss"], Tuple "password2" [Just "psss"]])
  pure unit
