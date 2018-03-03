module Data.Validation.Jaws.Http.Form where

import Prelude

import Control.Alt ((<|>))
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Op (Op(..))
import Data.Profunctor.Star (Star(..))
import Data.Record (get, insert)
import Data.StrMap (lookup)
import Data.Tuple (Tuple(..))
import Data.Validation.Jaws.Coproduct (CVBifunctor(..), CoproductValidation(..), check, check', pureV', runCoproductValidation)
import Data.Validation.Jaws.Http (Query, QueryField, addFieldFromQuery, int, int', nonEmptyString, scalar, scalar')
import Data.Validation.Jaws.Product (Builder(..), ProductValidation, Result(..))
import Data.Variant (Variant)
import Type.Prelude (class IsSymbol, class RowLacks, SProxy(..), reflectSymbol)

-- | This type is only used as html representation of validated values.
-- | Fully consitent and type safe values are additional result of validation.
data Form = Form (Array String) (Array Field)
derive instance genericForm ∷ Generic Form _
instance showForm ∷ Show Form where show = genericShow

instance semigroupForm ∷ Semigroup Form where
  append (Form e1 f1) (Form e2 f2)
    = Form (e1 <> e2) (f1 <> f2)

instance monoidForm ∷ Monoid Form where
  mempty = Form [] []

-- | Move to this representation
data FormValue a = Err String String | Val (Maybe a)
derive instance genericFormValue ∷ Generic (FormValue a) _
instance showFormValue ∷ (Show a) ⇒ Show (FormValue a) where show = genericShow

type Value = String
type Checked = Boolean
type Label = String
newtype Option = Option
  { value ∷ String
  , checked ∷ Boolean
  , label ∷ String
  }
derive instance newtypeOption ∷ Newtype Option _
derive instance genericOption ∷ Generic Option _
instance showOption ∷ Show Option where
  show = genericShow
type Options = Array Option
option v c l = Option { value: v, checked: c, label: l }

data Field
  = Input { label ∷ String, name ∷ String, value ∷ FormValue String }
  | Password { label ∷ String, name ∷ String, value ∷ FormValue String }
  | Number { label ∷ String, name ∷ String, value ∷ FormValue Int }
  -- | Radio { label ∷ String, name ∷ String, value ∷ Value Boolean }
  | Select { label ∷ String, name ∷ String, options ∷ Array (Tuple String String), value ∷ FormValue String}
  | Checkbox { label ∷ String, name ∷ String, value ∷ Either (Tuple String Options) Options }
derive instance genericField ∷ Generic Field _
instance showField ∷ Show Field where show = genericShow

-- | Just prototype of form library.

type FormInit a = a → Form

newtype FormValidation m query e a = FormValidation
  { validation ∷ CoproductValidation m (Tuple Form e) query (Tuple Form a)
  , init ∷ FormInit a
  }


inputFromRecordField p f =
  (\r → Form [] [ Input {  label: reflectSymbol p, name: reflectSymbol p, value: Val $ f (get p r) }])
numberFromRecordField p f =
  (\r → Form [] [ Number {  label: reflectSymbol p, name: reflectSymbol p, value: Val $ f (get p r) }])

inputFromRecordFieldReq p = inputFromRecordField p Just
inputFromRecordFieldOpt p = inputFromRecordField p id
numberFromRecordFieldReq p = numberFromRecordField p Just
numberFromRecordFieldOpt p = numberFromRecordField p id

addOption' ∷ ∀ b e i i' form l m tok v v'
  . (IsSymbol l)
  ⇒ (Monad m)
  ⇒ (RowCons l (Either _ _) i i')
  ⇒ (RowCons l _ v v')
  ⇒ (RowLacks l i)
  ⇒ (RowLacks l v)
  ⇒ (Functor m)
  ⇒ SProxy l
  → ProductValidation m _ (Tuple (Record i) _) (Tuple (Record i') _) (Tuple (Record v) _) (Tuple (Record v') _)
addOption' p =
  addForm p (addOption p)

opts = buildRecord $ addOption' (SProxy ∷ SProxy "first") >>> addOption' (SProxy ∷ SProxy "second") >>> addOption' (SProxy ∷ SProxy "third")

checkboxValue = pureV' (SProxy ∷ SProxy "on/off") (case _ of
  "on" → Right true
  "off" → Right false
  v → Left v)

addOption ∷ forall e l m.
  IsSymbol l
  ⇒ Monad m => (SProxy l) → CoproductValidation m
                     (Tuple
                        (Variant
                           ( scalar :: Array String
                           , "on/off" :: String
                           | e
                           )
                        )
                        (Array Option)
                     )
                     (Array String)
                     (Tuple Boolean (Array Option))
addOption l =
  (unwrap $ bimap (\r → Tuple r [ option (reflectSymbol l) false (reflectSymbol l) ]) (\r → Tuple r [ option (reflectSymbol l) r (reflectSymbol l) ]) <<< CVBifunctor $ (scalar' >>> checkboxValue)) -- <|> (missingValue >>> pure false)))
  --missingValue = check' (SProxy ∷ SProxy "missing") (case _ of
  --   Nothing → true
  --   Just "" → true
  --   _ → false)

x :: String -> Form
x = (\p → {password1: p, password2: p}) >>> (inputFromRecordFieldReq (SProxy ∷ SProxy "password1") <> (inputFromRecordFieldReq (SProxy ∷ SProxy "password2")))

y :: forall t265.
   { password :: String
   , bio :: Maybe String
   , age :: Maybe Int
   | t265
   }
   -> Form
y = inputFromRecordFieldOpt (SProxy ∷ SProxy "bio") <> numberFromRecordFieldOpt (SProxy ∷ SProxy "age") <> (_.password >>> x)

addFormFromQuery ∷ ∀ a e form m s ir ir' vr vr'
  . (IsSymbol s)
  ⇒ (Semigroup form)
  ⇒ (Monad m)
  ⇒ RowLacks s ir
  ⇒ RowCons s (Either e a) ir ir'
  ⇒ RowLacks s vr
  ⇒ RowCons s a vr vr'
  ⇒ SProxy s
  → (String → CoproductValidation m (Tuple e form) QueryField (Tuple a form))
  → ProductValidation m Query (Tuple (Record ir) form) (Tuple (Record ir') form) (Tuple (Record vr) form) (Tuple (Record vr') form)
addFormFromQuery p v =
  recordFieldValidation p (\query → runCoproductValidation (v (reflectSymbol p)) (fromMaybe [] (lookup (reflectSymbol p) query)))


recordFieldValidation ∷ ∀ b e i i' form l m tok v v'
  . (IsSymbol l)
  ⇒ Semigroup form
  ⇒ (Monad m)
  ⇒ (RowCons l (Either e b) i i')
  ⇒ (RowCons l b v v')
  ⇒ (RowLacks l i)
  ⇒ (RowLacks l v)
  ⇒ (Functor m)
  ⇒ SProxy l
  → (tok → m (Either (Tuple e form) (Tuple b form)))
  → ProductValidation m tok (Tuple (Record i) form) (Tuple (Record i') form) (Tuple (Record v) form) (Tuple (Record v') form)
recordFieldValidation p v =
  Builder (\tok a → toRecord a <$> (v tok))
 where
 toRecord (Valid (Tuple ir fi) (Tuple vr fv)) (Right (Tuple b f)) = Valid (Tuple (insert p (Right b) ir) (f <> fi)) (Tuple (insert p b vr) (f <> fv))
 toRecord (Valid (Tuple ir fi) _) (Left (Tuple e f)) = Invalid (Tuple (insert p (Left e) ir) (f <> fi))
 toRecord (Invalid (Tuple ir fi)) (Left (Tuple e f)) = Invalid (Tuple (insert p (Left e) ir) (f <> fi))
 toRecord (Invalid (Tuple ir fi)) (Right (Tuple e f)) = Invalid (Tuple (insert p (Right e) ir) (f <> fi))

input ∷ ∀ m
  . Monad m
  ⇒ String
  → String
  → CoproductValidation m (Tuple _ Form) QueryField (Tuple _ Form)
input label name =
  CoproductValidation v
 where
  CoproductValidation n = nonEmptyString
  v = n >>> Star (case _ of
    Left e → pure (Left (Tuple e (Form [] [Input { label, name, value: Err "Appropriate error message..." "" }])))
    Right v → pure (Right (Tuple v (Form [] [Input { label, name, value: Val (Just v) }]))))

password ∷ ∀ m
  . Monad m
  ⇒ String
  → String
  → CoproductValidation m (Tuple _ Form) QueryField (Tuple _ Form)
password label name =
  CoproductValidation v
 where
  CoproductValidation n = nonEmptyString
  v = n >>> Star (case _ of
    Left e → pure (Left (Tuple e (Form [] [Password { label, name, value: Err "Appropriate error message..." "" }])))
    Right v → pure (Right (Tuple v (Form [] [Password { label, name, value: Val (Just v)}]))))

number ∷ ∀ m
  . Monad m
  ⇒ String
  → String
  → CoproductValidation m (Tuple _ Form) QueryField (Tuple _ Form)
number label name =
  CoproductValidation v
 where
  CoproductValidation n = nonEmptyString >>> int'
  v = n >>> Star (case _ of
    Left e → pure (Left (Tuple e (Form [] [Number { label, name, value: Err "Appropriate error message..." "" }])))
    Right v → pure (Right (Tuple v (Form [] [Number { label, name, value: Val (Just v) }]))))

runRecordValidation ∷ ∀ i form m tok v
  . Monad m
  ⇒ Monoid form
  ⇒ ProductValidation m tok (Tuple {} form) (Tuple (Record i) form) (Tuple {} form) (Tuple (Record v) form)
  → (tok → m (Either (Tuple (Record i) form) (Tuple (Record v) form)))
runRecordValidation (Builder p) =
  (\tok → do
    r ← p tok (Valid (Tuple {} mempty) (Tuple {} mempty))
    case r of
      Invalid i → pure (Left i)
      Valid _ v → pure (Right v))

addForm ∷ ∀ b e i i' form l m tok v v'
  . (IsSymbol l)
  ⇒ (Monoid form)
  ⇒ (Monad m)
  ⇒ (RowCons l (Either e b) i i')
  ⇒ (RowCons l b v v')
  ⇒ (RowLacks l i)
  ⇒ (RowLacks l v)
  ⇒ (Functor m)
  ⇒ SProxy l
  → CoproductValidation m (Tuple e form) tok (Tuple b form)
  → ProductValidation m tok (Tuple (Record i) form) (Tuple (Record i') form) (Tuple (Record v) form) (Tuple (Record v') form)
addForm p v =
  recordFieldValidation p (runCoproductValidation v)

buildRecord ∷ ∀ i form m v tok
  . Monad m
  ⇒ (Monoid form)
  ⇒ ProductValidation m tok (Tuple {} form) (Tuple (Record i) form) (Tuple {} form) (Tuple (Record v) form)
  → CoproductValidation m (Tuple (Record i) form) tok (Tuple (Record v) form)
buildRecord = runRecordValidation >>> Star >>> CoproductValidation

