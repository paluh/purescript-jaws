# purescript-jaws

"Two dimentional" composable validation toolkit. Still in α-stage.

## Showtime

### Simple validation

Let's build password form.

User should provide non empty password twice...

  ```purescript
    import Data.Validation.Jaws.Record (addFieldQuery, buildRecord)
    import Data.Validation.Jaws.Validation (check, pureV, tag)

    passwordFields =
      buildRecord
       (addFieldFromQuery (SProxy ∷ SProxy "password1") nonEmptyString >>>
        addFieldFromQuery (SProxy ∷ SProxy "password2") nonEmptyString)
  ```

...and if they contain the same value...

  ```purescript
     passwordsEqual = check (\r → r.password1 == r.password2)
  ```
...we are accepting them as correct and build a password based on the value:

  ```purescript
   createPassword = pureV (_.password1 >>> Password >>> Right))
  ```
Let's combine these steps together and check them in action:

  ```
   password =
     (tag (SProxy ∷ SProxy "fields") passwordFields) >>>
     (tag (SProxy ∷ SProxy "passwordsEqual") (passwordsEqual >>> createPassword))

  ```

But before we validate something, few comments:

  * I'm using debug log (`traceAnyA`) in case of failure

  * we have used `Data.Validation.Jaws.Http.nonEmptyString` as entry point for our fields together with `addFieldFromQuery`
    so our validation is build upon `type Query = StrMap (Array (Maybe String))`

  * this library is not particularly tide to http validation

  * to run validation we just call `runValidation password queryData`


 I've written simple (and ugly) reporter which validates and prints the results:

  ```purescript
    validateAndPrint ∷ ∀ a e i eff m r. (Show i) ⇒ (Show r) ⇒ Validation (Eff (console ∷ CONSOLE | eff)) e i r → i → Eff (console ∷ CONSOLE | eff) Unit
    validateAndPrint v d = do
      r ←  runValidation v d
      case r of
        Right v → logShow (Right v ∷ Either Unit r)
        e → traceAnyA e
  ```

Let's validate:
  ```purescript
  -- both values missing
  validateAndPrint password empty
  ```

  ```purescript
    Left {
      value0:
       { type: 'fields',
         value:
          { password1: Left { value0: { type: 'nonEmpty', value: {} } },
            password2: Left { value0: { type: 'nonEmpty', value: {} } } } } }
  ```

  ```purescript
  -- one value missing
  validateAndPrint password (fromFoldable [Tuple "password1" [Just "admin"]])
  ```

  ```purescript
    Left {
      value0:
       { type: 'fields',
         value:
          { password1: Right { value0: 'admin' },
            password2: Left { value0: { type: 'nonEmpty', value: {} } } } } }

  ```

  ```purescript
  -- non equal passwords
  validateAndPrint password (fromFoldable [Tuple "password1" [Just "admin"], Tuple "password2" [Just "pass"]])
  ```

  ```purescript
    Left {
      value0:
       { type: 'equals',
         value: { password1: 'admin', password2: 'pass' } } }
  ```

  ```purescript
  -- correct data
  validateAndPrint password (fromFoldable [Tuple "password1" [Just "secret"], Tuple "password2" [Just "secret"]])
  ```

  ```purescript
    (Right "secret")
   ```

### Reusing existing validation

Let's assume that our registration process requires these data:

  ```purescript
    newtype Registration = Registration
      { nickname ∷ Nickname
      , email ∷ Email
      , password ∷ Password
      }
  ```

We can easily build validation reusing our existing `password` component:

  ```purescript
    registration =
      Registration <$> buildRecord
        ((addField (SProxy ∷ SProxy "password") password) >>>
         (addFieldFromQuery (SProxy ∷ SProxy "email") (nonEmptyString >>> email') >>>
         (addFieldFromQuery (SProxy ∷ SProxy "nickname") (nonEmptyString >>> pureV (Nickname >>> Right)))))
  ```
And like previously if we provide correct input we are going to get just plain value:

  ```purescript
      correctData =
        (fromFoldable
          [Tuple "password1" [Just "pass"],
           Tuple "password2" [Just "pass"],
           Tuple "email" [Just "email@example.com"],
           Tuple "nickname" [Just "nick"]])

    validateAndPrint registration correctData
  ```

  ```purescript
    (Registration { email: "email@example.com", nickname: "nick", password: "pass" })
  ```

but in case of invalid input...


  ```purescript
    passwordMismatch =
      (fromFoldable
        [Tuple "password1" [Just "wrong"],
         Tuple "password2" [Just "pass"],
         Tuple "email" [Just "email@example.com"],
         Tuple "nickname" [Just "nick"]])

    validateAndPrint registration passwordMismatch
  ```
...we are getting precise representation of failure but other data validated:

  ```purescript
    Left {
      value0:
       { password:
          Left {
            value0:
             { type: 'equals',
               value: { password1: 'wrong', password2: 'pass' } } },
         email: Right { value0: 'email@example.com' },
         nickname: Right { value0: 'nick' } } }
  ```
### Monadic validation

TODO

### Product and coproduct validation results

There are two types which are main building blocks for this library:

  ```purescript
    -- It's just (a → m (Either e b)) but those transformers
    -- give us so many things for free...
    newtype Validation m e a b = Validation (ReaderT a (ExceptT e m) b)
  ```

So `Validation` is nothing else as simple function from input to either error or correct output value. In this case we can consider result as sum type (sum of errors plus correct value type).

There is more complicated and involved type which aggregates validation errors/results into product type (`Record`) build upon this base:

  ```purescript
    -- | This is just (tok → a → m b)
    newtype Builder m tok a b = Builder (ReaderT {tok ∷ tok, a ∷ a} m b)
  ```

which has shape:

  ```purescript
    type RecordBuilder m tok i i' v v' =
      Builder m tok (Result (Record i) (Record v)) (Result (Record i') (Record v'))
  ```
You can argue that `Builder` and `Validation` can be generalized to common base, but belive me I was there and it wasn't fun.


## Conventions

  * All constructors with `'` at the end expects `SProxy l` as first arguments and do tagging step too

  * All combinators with `'` at the end are already tagged


## TODO

  * Add `Alt` instance for `Validation` and write optional `password` validation example

  * Use `Data.Record.Builder` internally

  * Provide utilities for handling failure values (paths to errors etc.)

  * Provide more basic validators for http
