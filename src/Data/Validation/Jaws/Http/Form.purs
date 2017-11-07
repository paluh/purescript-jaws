module Data.Validation.Jaws.Http.Form where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe, fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Profunctor.Star (Star(..))
import Data.Record (get, insert)
import Data.StrMap (lookup)
import Data.Tuple (Tuple(..))
import Data.Validation.Jaws.Coproduct (CoproductValidation(..), runCoproductValidation)
import Data.Validation.Jaws.Http (Query, QueryField, addFieldFromQuery, int, int', nonEmptyString)
import Data.Validation.Jaws.Product (Builder(..), ProductValidation, Result(..))
import Type.Prelude (class IsSymbol, class RowLacks, SProxy(..), reflectSymbol)

-- | Just prototype of form library.

-- | Move to this representation
-- data FormValue i v = Err String i | Val (Maybe v)
data FormValue a = Err String String | Val a
derive instance genericFormValue ∷ Generic (FormValue a) _
instance showFormValue ∷ (Show a) ⇒ Show (FormValue a) where show = genericShow

-- | This idea has been stolen from haskell `Turtle.Format`
data FormBuilder a b = FormBuilder ((Form → a) → b)

runFormBuilder (FormBuilder f) = f

instance semigroupoidFormBuilder ∷ Semigroupoid FormBuilder where
  compose (FormBuilder f2) (FormBuilder f1) = FormBuilder
    (\form2a →
        f2 (\form2 →
           f1 (\form1 →
              form2a (form1 <> form2))))

-- addInput :: forall t279 t285 t286 t287. IsSymbol t285 => RowCons t285 String t286 t287 => SProxy t285 -> FormBuilder { | t287 } t279 Field
addInput :: forall t287 t288 t289 t290. IsSymbol t287 => RowCons t287 String t288 t289 => SProxy t287 -> FormBuilder ({ | t289 } -> t290) ({ | t289 } -> t290)
addInput p =
  FormBuilder (\f2a → \r → (f2a (Form [] [ Input {  label: reflectSymbol p, name: reflectSymbol p, value: Val (get p r)} ]) r))
addNumber p =
  FormBuilder (\f2a → \r → (f2a (Form [] [ Number {  label: reflectSymbol p, name: reflectSymbol p, value: Val (get p r)} ]) r))

x :: forall t294 t304.
   FormBuilder
     ({ password1 :: String
      , password2 :: String
      | t304
      }
      -> t294
     )
     ({ password1 :: String
      , password2 :: String
      | t304
      }
      -> t294
     )
x = addInput (SProxy ∷ SProxy "password1") >>> (addInput (SProxy ∷ SProxy "password2"))

split :: forall t4 t5 t8.
   FormBuilder
     ({ password1 :: t4
      , password2 :: t4
      }
      -> t8
     )
     ({ password :: t4
      | t5
      }
      -> t8
     )
split =
  FormBuilder (\fr2a → \r → (fr2a (Form [] []) {password1: r.password, password2: r.password}))

y :: forall t356.
   { password :: String
   , bio :: String
   , age :: Int
   | t356
   }
   -> Form
y = runFormBuilder (addInput (SProxy ∷ SProxy "bio") <<< addNumber (SProxy ∷ SProxy "age") <<< (split <<< x)) (\f _ → f)

-- z :: Form
-- z = runFormBuilder (split <<< x) (\f _ → f) {password: "TEST"}

-- x = runFormBuilder (addInput (SProxy ∷ SProxy "password1") >>> (addInput (SProxy ∷ SProxy "password2"))) (\f _ → f) {password1: "test", password2: "fest"}

data Field
  = Input { label ∷ String, name ∷ String, value ∷ FormValue String }
  | Password { label ∷ String, name ∷ String, value ∷ FormValue String }
  | Number { label ∷ String, name ∷ String, value ∷ FormValue Int }
  -- | Radio { label ∷ String, name ∷ String, value ∷ Value Boolean }
  -- | Select { label ∷ String, name ∷ String, value ∷ Value (Array (Tuple String Boolean))}
  -- | Checkbox { label ∷ String, name ∷ String, value ∷ Value (Array (Tuple String Boolean))}
derive instance genericField ∷ Generic Field _
instance showField ∷ Show Field where show = genericShow

-- data Form = Form { errors ∷ Array String, fields ∷ Array Field }
-- 
-- instance semigroupForm ∷ Semigroup Form where
--   append (Form f1) (Form f2)
--     = Form
--       { errors: f1.errors <> f2.errors
--       , fields: f1.fields <> f2.fields }
-- 
-- instance monoidForm ∷ Monoid Form where
--   mempty = Form { errors: [], fields: [] }

data Form = Form (Array String) (Array Field)
derive instance genericForm ∷ Generic Form _
instance showForm ∷ Show Form where show = genericShow

instance semigroupForm ∷ Semigroup Form where
  append (Form e1 f1) (Form e2 f2)
    = Form (e1 <> e2) (f1 <> f2)

instance monoidForm ∷ Monoid Form where
  mempty = Form [] []

-- | This is just
-- data ProductBoomearng m tok i i' v v' = ProductBoomerang
--   { validation ∷ ProductValidation m tok i i' v v'
--   , serializer ∷ (Either i' v') → ((Either i v), tok → tok)
--   }
-- 

-- | I'm not sure about this representation...
-- data FormValidation m a b = FormValidation
--   { builder ∷ FormBuilder Form (a → Form)
--   , validation ∷ Builder m Query (Tuple a Form) (Tuple b Form)
--   }

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
    Right v → pure (Right (Tuple v (Form [] [Input { label, name, value: Val v}]))))

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
    Right v → pure (Right (Tuple v (Form [] [Password { label, name, value: Val v}]))))

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
    Right v → pure (Right (Tuple v (Form [] [Number { label, name, value: Val v }]))))

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

