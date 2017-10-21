# purescript-jaws

Simple validation library...


Picture is worth a thousend words... and is cheaper than real docs:


   ```purescript
   personForm ∷
     ∀ vr ir.
     TrackedValidation Query () (nickname ∷ _, age ∷ _, email ∷ _) () (nickname ∷ _, age ∷ _, email ∷ _)
   personForm =
     queryField (SProxy ∷ SProxy "nickname") (nonEmptyStr >=> (pure <<< Nickname)) >>>
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

outputs

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


