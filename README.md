# purescript-jaws

Simple validation library. Experimental phase.

## Example

Picture is worth a thousand words... and is cheaper than real docs ;-)


   ```purescript
   personForm ∷
     ∀ vr ir.
     TrackedValidation Query () (nickname ∷ _, age ∷ _, email ∷ _) () (nickname ∷ _, age ∷ _, email ∷ _)
   personForm =
     queryField (SProxy ∷ SProxy "nickname") (nonEmptyStr >=> (Nickname >>> pure)) >>>
     queryField (SProxy ∷ SProxy "age") (optional' (int >=> greaterThan 18)) >>>
     queryField (SProxy ∷ SProxy "email") (nonEmptyStr >=> email)

   main :: forall e. Eff (console :: CONSOLE | e) Unit
   main = do
     let
       validateAndPrint q = do
         case run personForm q of
           Right v → logShow (GoodPerson v)
           Left e → traceAnyA e
         log "\n"
     validateAndPrint empty
     validateAndPrint (fromFoldable [Tuple "nickname" [Just "paluh"], Tuple "age" [Just "666"], Tuple "email" [Just "paluho@gmail.com"]])
     validateAndPrint (fromFoldable [Tuple "nickname" [Just "paluh"], Tuple "age" [], Tuple "email" [Just "paluho@gmail.com"]])
     pure unit

   ```

outputs (errors are printed using `traceAnyA` - sorry):

   ```purescript
    { nickname: Left { value0: { type: 'missing', value: {} } },
      age: Right { value0: Nothing {} },
        email: Left { value0: { type: 'missing', value: {} } } }

    { nickname: Right { value0: 'paluh' },
      age: Left { value0: { type: 'toSmall', value: {} } },
        email: Left { value0: { type: 'emailFormat', value: {} } } }

    (GoodPerson { age: (Just 666), email: "paluho@gmail.com", nickname: "paluh" })

    (GoodPerson { age: Nothing, email: "paluho@gmail.com", nickname: "paluh" })
   ```

## Internals

To be honest there is really not much inside.

There is validation category which aggregates results:

  ```purescript
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
  ```

It is used to aggregate validation steps into final records containing only valid values or valid values and invalid ones wrapped in `Either`. Single step ads single field to the result.

On top of it there is `TrackedValidation` type synonym which possibly passes two versions of result (when current and previous steps succeeded) which simpifies both possible scenarios:

   ```purescript
    type TrackedValidation tok i i' v v' =
      Validation
        tok
        (Either (Record i) { v ∷ Record v, i ∷ Record i})
        (Either (Record i') { v ∷ Record v', i ∷ Record i'})
   ```

Additionally I'm using `Variant` (from wonderful `purescript-variants`) for extensible errors handling.

## TODO

  * Add type class to convert record of validators into `TrackedValidation`, so we can probably omit type anotations (which seems necessary in case of composition)...

  * Split it into submodules

  * Add more validators

  * Provide example scenario with two step validation (two password inputs)
