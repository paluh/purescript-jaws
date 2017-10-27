# purescript-jaws

"Two dimentional" composable validation toolkit. Still in α-stage.

## Usage

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
     (tag (SProxy ∷ SProxy "equals") (passwordsEqual >>> createPassword))

  ```

When you see `SProxy :: SProxy label` you can think of a labeling validation step. Labeling steps is necessary in case of "sum validation" - where validation is chained one after another. It's basically `a -> Either e b` chain where every `e` can and should be labeled (thanks to `purescript-variant`!) to form single coproduct type.

On the other hand we have also products/records build up steps. This kind of validation steps aggregates all results into product (through `addField` or dedicated query helper `addFieldFromQuery`). In this context labeling is just giving record field a name. Resulting product value would contain all valid, but also invalid values (values wrapped in `Either`), but if the whole validation processes passes you would get record without any additional wrapping!

Few additional comments regarding following examples:

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
  -- one value missing - errors occures during `password2.fields` and `password1.fields` steps
    Left {
      value0:
       { type: 'fields',
         value:
          { password1: Left { value0: { type: 'nonEmpty', value: {} } },
            password2: Left { value0: { type: 'nonEmpty', value: {} } } } } }
  ```

  ```purescript
  -- one value missing - error occures during `password2.fields` step
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
  -- non equal passwords - error occures during `equals` step
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
         (addFieldFromQuery (SProxy ∷ SProxy "nickname") (Nickname <$> nonEmptyString))))
  ```
And like previously if we provide correct input we are going to get just plain value:

  ```purescript
      correctData =
        (fromFoldable
          [ Tuple "password1" [Just "pass"]
          , Tuple "password2" [Just "pass"]
          , Tuple "email" [Just "email@example.com"]
          , Tuple "nickname" [Just "nick"]])

    validateAndPrint registration correctData
  ```

  ```purescript
    (Registration { email: "email@example.com", nickname: "nick", password: "pass" })
  ```

but in case of invalid input...


  ```purescript
    passwordMismatch =
      (fromFoldable
        [ Tuple "password1" [Just "wrong"]
        , Tuple "password2" [Just "pass"]
        , Tuple "email" [Just "email@example.com"]
        , Tuple "nickname" [Just "nick"]])

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

### Tweaking

Now let's consider a bit more difficult reusability scenario - we want to provide profile edit form:

  ```purescript
    newtype Profile = Profile
      { nickname ∷ Nickname
      , bio ∷ Maybe String
      , age ∷ Maybe Int
      , password ∷ Maybe Password
      }
    derive instance genericProfile ∷ Generic Profile _
    instance showProfile ∷ Show Profile where
      show = genericShow
  ```
But in this context we want to validate also case where user leaves two passwords empty. In that case
we don't update it's value in our db.

To add this additional scenario we should bulid validator which passes when two values from query (just a recap - `type Query = StrMap (Array (Maybe String))`) are empty.
In this case our final password value should be `Nothing`.
As usual we are going to use `pureV` constructor which has type `pureV ∷ (Monad m) ⇒ (a → Either e b) → Validation m e a b` in other words it lifts simple validation function
into our validation monad stack. `a` type is an result from previous validation steps.

Lets build this simple combinator from scratch:

  ```purescript
   missingValue ∷ ∀ a m. Monad m ⇒ String → Validation m Unit Query Query
   missingValue p = check (\query → case lookup p query of
      Nothing → true
      Just [Nothing] → true
      Just [Just ""] → true
      _ → false)
  ```
WAT? Yes, we are considering these THREE values as emtpy ;-)

Now we can validate both passwords using above combinator to form final validation:

  ```purescript
    emptyPasswords ∷ ∀ a m. Monad m ⇒ Validation m Unit Query (Maybe a)
    emptyPasswords = (missingValue "password1" >>> missingValue "password2" >>> pure Nothing)
  ```

We can read above `Validation` (there is also complementary type for "product validation") signature as follows:

  * `m` - monad which we are using as the context for validation
  * `Unit` - error type
  * `Query` - previous validation step result
  * `Maybe a` - successful validation result type (this `a` polimorphism doesn't hurt here ;-))


and tag this step too:

  ```purescript
    emptyPasswords' = tag (SProxy "emptyPasswords") emptyPasswords
  ```

TODO: more docs soon...

  ```purescript
    profile =
      Profile <$>
        (buildRecord
          ((addField (SProxy ∷ SProxy "password") (emptyPasswords' <|> (Just <$> password)) >>>
            addFieldFromQuery (SProxy ∷ SProxy "bio") (scalar <|> pure Nothing) >>>
            addFieldFromQuery (SProxy ∷ SProxy "age") (catMaybesV >>> optional int') >>>
            addFieldFromQuery (SProxy ∷ SProxy "nickname") (Nickname <$> nonEmptyString)))
  let
    onlyNickname =
      (fromFoldable
        [Tuple "nickname" [Just "nick"]])

  validateAndPrint profile onlyNickname
  ```

  ```purescript
  (Right (Profile { age: Nothing, bio: Nothing, nickname: "nick", password: Nothing }))
  ```

  ```purescript
  let
    nicknameAndPassword =
      (fromFoldable
        [ Tuple "nickname" [Just "nick"]
        , Tuple "password1" [Just "new"]
        , Tuple "password2" [Just "new"]])
  ```

  ```purescript
  validateAndPrint profile nicknameAndPassword
  ```


  ```purescript
  (Right (Profile { age: Nothing, bio: Nothing, nickname: "nick", password: (Just "new") }))
  ```

  ```purescript
  let
    nicknameAndPasswordMismatch =
      (fromFoldable
        [ Tuple "nickname" [Just "nick"]
        , Tuple "password1" [Just "wrong"]
        , Tuple "password2" [Just "new"]])

  validateAndPrint profile nicknameAndPasswordMismatch
  ```

  ```purescript
    Left {
      value0:
       { password:
          Left {
            value0:
             { type: 'equals',
               value: { password1: 'wrong', password2: 'new' } } },
         bio: Right { value0: Nothing {} },
         age: Right { value0: Nothing {} },
         nickname: Right { value0: 'nick' } } }
  ```



### Monadic validation

TODO

## Show me the types

There are two types which are main building blocks for this library:

  ```purescript
    -- It's just (a → m (Either e b)) but those transformers
    -- give us so many things for free...
    newtype Validation m e a b = Validation (ReaderT a (ExceptT e m) b)
  ```

So `Validation` is nothing else as simple function from input to either error or correct output value. In this case we can consider result as sum type (sum of errors plus correct value type).

What is really important we can write and `Semigroupoid` and `Category` instances for this type as follows:


  ```purescript
    instance semigroupoidValidation ∷ (Monad m) ⇒ Semigroupoid (Validation m e) where
      compose (Validation v2) (Validation v1) =
        Validation (ReaderT (runReaderT v1 >=> runReaderT v2))

    instance categoryValidation ∷ (Monad m) ⇒ Category (Validation m e) where
      id = Validation (ReaderT pure)
  ```

So it is just an Kleisli Arrow for our ReaderT underlining monad.


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

TODO: More about `Builder` category and monad stack soon ;-)


## Conventions

  * All constructors with `'` at the end expects `SProxy l` as first arguments and do tagging step too

  * All combinators with `'` at the end are already tagged


## TODO

  * Add `password` validation example with `<|>` usage

  * Use `Data.Record.Builder` internally

  * Provide more basic validators for http

  * Test `HStack` as an alternative for records

