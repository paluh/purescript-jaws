module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (empty, fromFoldable)
import Data.String (Pattern(..), contains)
import Data.Symbol (SProxy(SProxy))
import Data.Tuple (Tuple(..))
import Data.Validation.Jaws.Http (addFieldFromQuery, nonEmptyString)
import Data.Validation.Jaws.Record (addField, buildRecord, buildRecord', combine)
import Data.Validation.Jaws.Validation (PureValidation, Validation, check, check', pureV, runValidation, tag)
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
-- All Validation constructors which have ampersand at the end
-- are taking symbol as and wraps error in Variant
email' ∷ ∀ v. PureValidation (Variant (email ∷ String | v)) String  Email
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


registration =
  Registration <$> buildRecord
    ((addField (SProxy ∷ SProxy "password") password) >>>
     (addFieldFromQuery (SProxy ∷ SProxy "email") (nonEmptyString >>> email') >>>
     (addFieldFromQuery (SProxy ∷ SProxy "nickname") (nonEmptyString >>> pureV (Nickname >>> Right)))))

validateAndPrint ∷ ∀ a b e eff v
  . Show b
  ⇒ Validation (Eff (console ∷ CONSOLE  | eff)) e a b
  → a
  → Eff (console ∷ CONSOLE | eff) Unit
validateAndPrint f q = do
  r ← runValidation f q
  case r of
    Right v → logShow v
    Left e → traceAnyA e

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let
    validatePassword v = do
      log ("Vaildation of value: " <> show v)
      r ← runValidation password v
      case r of
        Right r → log ("Passed: " <> unwrap r)
        Left e → do
          (case_
            # on (SProxy ∷ SProxy "fields") (log <<< (const "Failed on step: 'fields'"))
            # on (SProxy ∷ SProxy "equals") (log <<< (const "Failed on step: 'equals'"))) e
          log "Trace of value:"
          traceAnyA e
          log "\n"

  validatePassword empty
  validatePassword (fromFoldable [Tuple "password1" [Just "admin"]])
  validatePassword (fromFoldable [Tuple "password1" [Just "admin"], Tuple "password2" [Just "pass"]])
  validatePassword (fromFoldable [Tuple "password1" [Just "secret"], Tuple "password2" [Just "secret"]])

  -- validateAndPrint
  --   registration
  --   (fromFoldable [Tuple "password1" [Just "pass"], Tuple "password2" [Just "pass"]])

  -- validateAndPrint
  --   registration
  --   (fromFoldable
  --     [Tuple "password1" [Just "pass"],
  --      Tuple "password2" [Just "pass"],
  --      Tuple "email" [Just "email@example.com"],
  --      Tuple "nickname" [Just "nick"]])
