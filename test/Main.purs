module Test.Main where

import Prelude

import Control.Alternative ((<|>))
import Control.Error.Util (note)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.StrMap (StrMap, empty, fromFoldable, lookup)
import Data.String (Pattern(..), contains)
import Data.Symbol (SProxy(SProxy))
import Data.Tuple (Tuple(..))
import Data.Validation.Jaws (addField, buildRecord)
import Data.Validation.Jaws.Coproduct (CoproductValidation, check, check', pureV, pureV', runCoproductValidation, tag)
import Data.Validation.Jaws.Http (Query, addFieldFromQuery, catMaybesV, int, int', nonEmptyArray, nonEmptyArray', nonEmptyString, optional, scalar, scalar')
import Data.Validation.Jaws.Http.Form (y)
import Data.Validation.Jaws.Product (recordFieldValidation)
import Data.Variant (Variant, on, case_)
import Debug.Trace (traceAnyA)

newtype Email = Email String
derive instance newtypeEmail ∷ Newtype Email _
derive newtype instance showShow ∷ Show Email

newtype Nickname = Nickname String
derive instance newtypeNickname ∷ Newtype Nickname _
derive newtype instance showNickname ∷ Show Nickname

newtype Password = Password String
derive instance newtypePassword ∷ Newtype Password _
derive newtype instance showPassword ∷ Show Password

newtype Registration = Registration
  { nickname ∷ Nickname
  , email ∷ Email
  , password ∷ Password
  }
derive instance genericRegistration ∷ Generic Registration _
instance showRegistration ∷ Show Registration where
  show = genericShow

-- This is how we create validation
-- All CoproductValidation constructors which have ampersand at the end
-- are taking symbol as and wraps error in Variant
email' ∷ ∀ m v. (Monad m) ⇒ CoproductValidation m (Variant (email ∷ String | v)) String  Email
email' = check' (SProxy ∷ SProxy "email") (contains (Pattern "@")) >>> pureV (Email >>> Right)

passwordFields =
  buildRecord
    (addFieldFromQuery (SProxy ∷ SProxy "password1") nonEmptyString >>>
     addFieldFromQuery (SProxy ∷ SProxy "password2") nonEmptyString)

passwordsEqual = check (\r → r.password1 == r.password2)

createPassword = pureV (_.password1 >>> Password >>> Right)

password =
  (tag (SProxy ∷ SProxy "fields") passwordFields) >>>
  (tag (SProxy ∷ SProxy "equals") (passwordsEqual >>> createPassword))

registration :: forall m. Monad m => CoproductValidation m _ Query Registration
registration =
  Registration <$>
    (buildRecord
      ((addField (SProxy ∷ SProxy "password") password) >>>
       (addFieldFromQuery (SProxy ∷ SProxy "email") (nonEmptyString >>> email') >>>
       (addFieldFromQuery (SProxy ∷ SProxy "nickname") (nonEmptyString >>> pureV (Nickname >>> Right))))))


newtype Profile = Profile
  { nickname ∷ Nickname
  , bio ∷ Maybe String
  , age ∷ Maybe Int
  , password ∷ Maybe Password
  }
derive instance genericProfile ∷ Generic Profile _
instance showProfile ∷ Show Profile where
  show = genericShow


-- missingValue :: ∀ a m. (Monad m) ⇒ String → CoproductValidation m Unit Query Query
missingValue p = check (\query → case lookup p query of
   Nothing → true
   (Just [Nothing]) → true
   (Just [Just ""]) → true
   _ → false)

emptyPasswords =
   missingValue "password1" >>> missingValue "password2" >>> pure Nothing

emptyPasswords' = tag (SProxy ∷ SProxy "empty") emptyPasswords

-- profile :: forall a e m. Monad m => CoproductValidation m e a _
profile =
  Profile <$>
    (buildRecord
      (addField (SProxy ∷ SProxy "password") (emptyPasswords' <|> (Just <$> password)) >>>
       addFieldFromQuery (SProxy ∷ SProxy "bio") (scalar <|> pure Nothing) >>>
       addFieldFromQuery (SProxy ∷ SProxy "age") (catMaybesV >>> optional int') >>>
       addFieldFromQuery (SProxy ∷ SProxy "nickname") (Nickname <$> nonEmptyString)))

validateAndPrint ∷ ∀ a e i eff m r. (Show i) ⇒ (Show r) ⇒ CoproductValidation (Eff (console ∷ CONSOLE | eff)) e i r → i → Eff (console ∷ CONSOLE | eff) Unit
validateAndPrint v d = do
  log ("Validating :" <> show d)

  r ←  runCoproductValidation v d
  case r of
    Right v → logShow (Right v ∷ Either Unit r)
    e → traceAnyA e
  log "\n"



passwordFieldsWithPath =
  buildRecord
    (addFieldFromQuery (SProxy ∷ SProxy "password1") nonEmptyString >>>
     addFieldFromQuery (SProxy ∷ SProxy "password2") nonEmptyString)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  validateAndPrint password empty
  validateAndPrint password (fromFoldable [Tuple "password1" [Just "admin"]])
  validateAndPrint password (fromFoldable [Tuple "password1" [Just "admin"], Tuple "password2" [Just "pass"]])
  validateAndPrint password (fromFoldable [Tuple "password1" [Just "secret"], Tuple "password2" [Just "secret"]])

  let
    correct =
      (fromFoldable
        [ Tuple "password1" [Just "pass"]
        , Tuple "password2" [Just "pass"]
        , Tuple "email" [Just "email@example.com"]
        , Tuple "nickname" [Just "nick"]])
    passwordMismatch =
      (fromFoldable
        [ Tuple "password1" [Just "wrong"]
        , Tuple "password2" [Just "pass"]
        , Tuple "email" [Just "email@example.com"]
        , Tuple "nickname" [Just "nick"]])

  validateAndPrint registration passwordMismatch

  validateAndPrint registration correct

  let
    onlyNickname =
      (fromFoldable
        [Tuple "nickname" [Just "nick"]])
  validateAndPrint profile onlyNickname

  let
    nicknameAndPassword =
      (fromFoldable
        [ Tuple "nickname" [Just "nick"]
        , Tuple "password1" [Just "new"]
        , Tuple "password2" [Just "new"]])

  validateAndPrint profile nicknameAndPassword

  let
    nicknameAndPasswordMismatch =
      (fromFoldable
        [ Tuple "nickname" [Just "nick"]
        , Tuple "password1" [Just "wrong"]
        , Tuple "password2" [Just "new"]])

  validateAndPrint profile nicknameAndPasswordMismatch

  logShow (y {password: "test", age: 8, bio: "my life is a mistake"})

