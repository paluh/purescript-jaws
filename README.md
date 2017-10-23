# purescript-jaws

Chew them all, but... keep those pesky Vegans outside!

`<img src="Spilberg to the rescue" />`

---

Simple, but __composable__ validation toolkit without type class mangling!

Project is still in experimental phase - I'm currently rewriting it to provide more principled types and more systematic way of error handling, monadic validation and... documentation of course :-P

## Example

Picture is worth a thousand words... and is cheaper than real docs ;-)


   ```purescript

   -- Unfortunatelly currently when building `Validation` through composition type annotation is required for
   -- record structure like below :-(

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

Above snippet outputs (errors are printed using `traceAnyA` - sorry):

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

## TODO

  * Add type class to convert record of validators into `TrackedValidation`, so we can probably omit type anotations (which seems necessary in case of composition)...

  * Split it into submodules

  * Add more validators

  * Provide example scenario with two step validation (two password inputs)
