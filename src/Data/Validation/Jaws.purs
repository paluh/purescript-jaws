module Data.Validation.Jaws where

import Prelude

import Data.Either (Either)
import Data.Profunctor.Star (Star(..))
import Data.Validation.Jaws.Coproduct (CoproductValidation(..), runCoproductValidation, tag)
import Data.Validation.Jaws.Product (ProductValidation, recordFieldValidation, runRecordValidation)
import Data.Variant (Variant)
import Type.Prelude (class IsSymbol, class RowLacks, SProxy)

addField ∷ ∀ b e i i' l m tok v v'
  . (IsSymbol l)
  ⇒ (Monad m)
  ⇒ (RowCons l (Either e b) i i')
  ⇒ (RowCons l b v v')
  ⇒ (RowLacks l i)
  ⇒ (RowLacks l v)
  ⇒ (Functor m)
  ⇒ SProxy l
  → CoproductValidation m e tok b
  → ProductValidation m tok (Record i) (Record i') (Record v) (Record v')
addField p v =
  recordFieldValidation p (runCoproductValidation v)

buildRecord ∷ ∀ i m v tok
  . Monad m
  ⇒ ProductValidation m tok {} (Record i) {} (Record v)
  → CoproductValidation m (Record i) tok (Record v)
buildRecord = runRecordValidation >>> Star >>> CoproductValidation

buildRecord' ∷ ∀ i l m v r r' tok
  . Monad m
  ⇒ IsSymbol l
  ⇒ RowCons l (Record i) r r'
  ⇒ SProxy l
  → ProductValidation m tok {} (Record i) {} (Record v)
  → CoproductValidation m (Variant r') tok (Record v)
buildRecord' p = buildRecord >>> tag p
