# purescript-jaws

"Two dimentional" composable validation toolkit. Still in α-stage.

## Showtime

### Simple validation

Let's build password form. User should provide non empty passwords twice...

  ```purescript
    import Data.Validation.Jaws.Record (addFieldQuery, buildRecord)

    passwordFields =
      buildRecord
       (addFieldFromQuery (SProxy ∷ SProxy "password1") nonEmptyString >>>
        addFieldFromQuery (SProxy ∷ SProxy "password2") nonEmptyString)
  ```

...and if they contain the same value...

  ```purescript
     passwordsEqual = check (\r → r.password1 == r.password2)
  ```
...we are accepting them as correct and build a password:

  ```purescript
   createPassword = pureV (_.password1 >>> Password >>> Right))
  ```
Let's combine these steps together and check them in action:

  ```
   password =
     (tag (SProxy ∷ SProxy "fields") passwordFields) >>>
     (tag (SProxy ∷ SProxy "passwordsEqual") (passwordsEqual >>> createPassword))

  ```

But before we validate something few comments:

  * to run validation we just call `runValidation password queryData`

  * I'm using debug log (`traceAnyA`) in case of failure

  * we have used `Data.Validation.Jaws.Http.nonEmptyString` as entry point for our fields together with `addFieldFromQuery`
    so our validation is build upon `type Query = StrMap (Array (Maybe String))`

  * this library is not particularly tide to http validation


 I've written simple reporter which validates and prints the results - it is not really important (check `tests/Main.purs`) in this context.
 Let's validate:

  ```purescript
    validatePassword v = do
      log ("Vaildation of value: " <> show v)
      ...

    main = do
      validatePassword empty
      validatePassword (fromFoldable [Tuple "password1" [Just "admin"]])
      validatePassword (fromFoldable [Tuple "password1" [Just "admin"], Tuple "password2" [Just "pass"]])
      validatePassword (fromFoldable [Tuple "password1" [Just "secret"], Tuple "password2" [Just "secret"]])
   ```

   ```
    Vaildation of value: (fromFoldable [])
    Failed on step: 'fields'
    Trace of value:
    { type: 'fields',
      value:
       { password1: Left { value0: { type: 'nonEmpty', value: {} } },
         password2: Left { value0: { type: 'nonEmpty', value: {} } } } }


    Vaildation of value: (fromFoldable [(Tuple "password1" [(Just "admin")])])
    Failed on step: 'fields'
    Trace of value:
    { type: 'fields',
      value:
       { password1: Right { value0: 'admin' },
         password2: Left { value0: { type: 'nonEmpty', value: {} } } } }


    Vaildation of value: (fromFoldable [(Tuple "password1" [(Just "admin")]),(Tuple "password2" [(Just "pass")])])
    Failed on step: 'equals'
    Trace of value:
    { type: 'equals',
      value: { password1: 'admin', password2: 'pass' } }


    Vaildation of value: (fromFoldable [(Tuple "password1" [(Just "secret")]),(Tuple "password2" [(Just "secret")])])
    Passed: secret
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

### Monadic validation

TODO

### Product and coproduct validation results

There are two types which are main building blocks for this library:

  ```purescript
    -- It's just (a → m (Either e b)) but those transformers
    -- give us so many things for free...
    newtype Validation m e a b = Validation (ReaderT a (ExceptT e m) b)
  ```

So validation is nothing else as simple function from input to either error or correct output value. In this case we can consider result as sum type (sum of errors plus correct value type).

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
You can argue that `Builder` and `Validation` can be generalized to common base, but belive me I was there and it wasn't nice.


## TODO

  * Use `Data.Record.Builder` internally

  * Provide utilities for handling failure values (paths to errors etc.)

  * Provide more basic validators for http
