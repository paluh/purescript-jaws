module Data.Validation.Jaws.Coproduct where

import Prelude

import Control.Alt (class Alt)
import Data.Either (Either(Left, Right))
import Data.EitherR (fmapL)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)

newtype CoproductValidation m e a b = CoproductValidation (Star m a (Either e b))
derive instance newtypeCoproductVaildation ∷ Newtype (CoproductValidation m e a b) _
derive instance functorCoproductValidation ∷ (Functor m) ⇒ Functor (CoproductValidation m e a)

instance bindCoproductValidation ∷ (Monad m) ⇒ Bind (CoproductValidation m e a) where
  bind (CoproductValidation v) f =
    CoproductValidation <<< Star $ (\a → do
      r ← unwrap v a
      case r of
        Left e → pure $ Left e
        Right u →
          let CoproductValidation v' = f u
          in unwrap v' a)

instance applyCoproductValidation ∷ (Monad m) ⇒ Apply (CoproductValidation m e a) where
  apply vf va = do
    f ← vf
    a ← va
    pure (f a)

instance applicativeCoproductValidation ∷ (Monad m) ⇒ Applicative (CoproductValidation m e a) where
  pure v = CoproductValidation <<< Star $ (\a → pure (Right v))

instance monadCoproductValidation ∷ (Monad m) ⇒ Monad (CoproductValidation m e a)

runCoproductValidation ∷ ∀ a b e m. CoproductValidation m e a b → a → m (Either e b)
runCoproductValidation (CoproductValidation v) = unwrap v

-- XXX: Should we sum errors and... force evaluation of all validators?
instance altCoproductValidation ∷ (Monad m) ⇒ Alt (CoproductValidation m e a) where
  alt v1 v2 = CoproductValidation <<< Star $ (\a → do
    eb <- runCoproductValidation v1 a
    case eb of
      r@(Right b) → pure r
      _ → runCoproductValidation v2 a)

instance semigroupoidCoproductValidation ∷ (Monad m) ⇒ Semigroupoid (CoproductValidation m e) where
  compose (CoproductValidation v2) (CoproductValidation v1) =
    CoproductValidation <<< Star $ (\a → do
      eb ← unwrap v1 a
      case eb of
        Right b → unwrap v2 b
        Left e → pure (Left e))

instance categoryCoproductValidation ∷ (Monad m) ⇒ Category (CoproductValidation m e) where
  id = CoproductValidation <<< Star $ (pure <<< Right)

instance profunctorCoproductValidation ∷ (Monad m) ⇒ Profunctor (CoproductValidation m e) where
  dimap f g (CoproductValidation v) =
    -- we have to work over `Either e` not over whole `Either e a`
    CoproductValidation <<< Star $ (f >>> pure) >=> unwrap v >=> (map g >>> pure)

instance choiceCoproductValidation ∷ (Monad m) ⇒ Choice (CoproductValidation m e) where
  left (CoproductValidation (Star v)) =
    CoproductValidation <<< Star $ v'
   where
    v' (Right r) = pure (Right (Right r))
    v' (Left input) = (Left <$> _) <$> v input
  right (CoproductValidation (Star v)) =
    CoproductValidation <<< Star $ v'
   where
    v' (Left r) = pure (Right (Left r))
    v' (Right input) = (Right <$> _) <$> (v input)

instance strongCoproductValidation ∷ (Monad m) ⇒ Strong (CoproductValidation m e) where
  first (CoproductValidation (Star v)) =
    CoproductValidation <<< Star $ v'
   where
    v' (Tuple input c) = ((flip Tuple c) <$> _) <$> (v input)
  second (CoproductValidation (Star v)) =
    CoproductValidation <<< Star $ v'
   where
    v' (Tuple c input) = ((Tuple c) <$> _) <$> (v input)


-- | Beside these constructors you can also use just Applicative `pure`
validation ∷ ∀ a b e m. (a → m (Either e b)) → CoproductValidation m e a b
validation = CoproductValidation <<< Star

validation' ∷ forall a b e m p r r'
  . RowCons p e r r'
  ⇒ Monad m
  ⇒ IsSymbol p
  ⇒ SProxy p
  → (a → m (Either e b))
  → CoproductValidation m (Variant r') a b
validation' l f = tag l (validation f)

pureV ∷ ∀ a b e m. (Monad m) ⇒ (a → Either e b) → CoproductValidation m e a b
pureV f = CoproductValidation <<< Star $ (pure <<< f)

pureV' ∷ forall a b e m p r r'
  . RowCons p e r r'
  ⇒ Monad m
  ⇒ IsSymbol p
  ⇒ SProxy p
  → (a → Either e b)
  → CoproductValidation m (Variant r') a b
pureV' l f = tag l (pureV f)

fail ∷ ∀ a b e m. (Monad m) ⇒ e → CoproductValidation m e a b
fail = CoproductValidation <<< Star <<< const <<< pure <<< Left

fail' ∷ ∀ a b e m p r r'
  . RowCons p e r r'
  ⇒ Monad m
  ⇒ IsSymbol p
  ⇒ SProxy p
  → e
  → CoproductValidation m (Variant r') a b
fail' p = tag p <<< CoproductValidation <<< Star <<< const <<< pure <<< Left

tag :: forall a b e m p r r'
  . RowCons p e r r'
  ⇒ Monad m
  ⇒ IsSymbol p
  ⇒ SProxy p
  → CoproductValidation m e a b
  → CoproductValidation m (Variant r') a b
tag p (CoproductValidation (Star v)) =
  CoproductValidation <<< Star $ ((fmapL (inj p) <$> _) <$> v)

check :: forall a m
  . (Monad m)
  ⇒ (a → Boolean)
  → CoproductValidation m a a a
check c = (pureV $ \a →
  if c a
    then (Right a)
    else (Left a))

check' :: forall a l m r r'
  . RowCons l a r r'
  ⇒ Monad m
  ⇒ IsSymbol l
  ⇒ SProxy l
  → (a → Boolean)
  → CoproductValidation m (Variant r') a a
check' t = check >>> tag t

