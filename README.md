# purescript-jaws

Purescript validation library experiment.


   ```purescript
   -- With above machinery we can define form validator by composition of validators
   -- and simple call: `validate personForm query`.
   -- Unfortunatelly I'm not sure if it is possible drop somehow these type anotations and
   -- they are quite huge as I'm tracking to paths...
   personForm ∷
     ∀ vr ir.
     Validator
       Query
       (Either {} { v ∷ {}, i ∷ {}})
       (Either
         { nickname ∷ _, age ∷ _, email ∷ _ }
         { v ∷ { nickname ∷ _, age ∷ _, email ∷ _ }
         , i ∷ { nickname ∷ _, age ∷ _, email ∷ _ }})
   personForm =
     queryValidator (SProxy ∷ SProxy "nickname") (required >=> (pure <<< Nickname)) >>>
     queryValidator (SProxy ∷ SProxy "age") (optional' (int >=> greaterThan 18)) >>>
     queryValidator (SProxy ∷ SProxy "email") (required >=> email)
   ```
