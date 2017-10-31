module Data.Validation.Jaws.Coproduct where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Monad.Except.Trans (class MonadThrow, throwError)
import Data.Bifunctor (class Bifunctor, lmap)
import Data.Either (Either(Left, Right))
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)

-- | If you want to treat your your input data as sum (~ Either a b)
-- | you have to provide morphisms to your final type.
-- | Category instance of this types "traverses" results of this morphisms
-- | until it finds appropriate value (one of errors or one of correct values).
-- |
-- | I've left `s` type as parameter (but in general this would be `Either`)
-- | so you  are free to use `Applicative` with monoidal error interface
-- | (like `V` from `purescript-validation`) for simpler transformations scenarios.
newtype CoproductValidation m s e a b = CoproductValidation (a → m (s e b))
derive instance functorCoproductValidation ∷ (Functor m, Functor (s e)) ⇒ Functor (CoproductValidation m s e a)

instance applyCoproductValidation ∷ (Monad m, Applicative (s e)) ⇒ Apply (CoproductValidation m s e a) where
  -- | We are passing here input `i` value between "steps", so this are morphisms
  -- | from input to multiple values... so this interface can bee seen as a
  -- | product "builder" (from categorical point of view), but product
  -- | carrier itself which combines these partial results has to be provided by
  -- | "External Force" ;-)
  apply (CoproductValidation vf) (CoproductValidation va) = CoproductValidation (\i → vf i >>= \f → va i >>= \a → pure (f <*> a))

instance applicativeCoproductValidation ∷ (Monad m, Applicative (s e)) ⇒ Applicative (CoproductValidation m s e a) where
  pure v = CoproductValidation (\a → pure (pure v))

instance bindCoproductValidation ∷ (Monad m) ⇒ Bind (CoproductValidation m Either e a) where
  bind (CoproductValidation v) f =
    CoproductValidation (\a → do
      r ← v a
      case r of
        Left e → pure $ Left e
        Right u →
          let CoproductValidation v' = f u
          in v' a)

runCoproductValidation ∷ ∀ a b e m s. CoproductValidation m s e a b → a → m (s e b)
runCoproductValidation (CoproductValidation v) = v

instance monadCoproductValidation ∷ (Monad m) ⇒ Monad (CoproductValidation m Either e a)

-- | I'm really not sure about this instance as it is gready
-- | and evaluates both cases. What do you think?
instance altCoproductValidation ∷ (Monad m, Alt (s e)) ⇒ Alt (CoproductValidation m s e a) where
  alt v1 v2 = CoproductValidation (\a → do
    eb <- runCoproductValidation v1 a
    eb'<- runCoproductValidation v2 a
    pure (eb <|> eb'))

instance semigroupoidCoproductValidation ∷ (Monad m) ⇒ Semigroupoid (CoproductValidation m Either e) where
  compose (CoproductValidation v2) (CoproductValidation v1) =
    CoproductValidation (\a → do
      eb ← v1 a
      case eb of
        Right b → v2 b
        Left e → pure (Left e))

instance categoryCoproductValidation ∷ (Monad m, Applicative (s e)) ⇒ Category (CoproductValidation m Either e) where
  id = CoproductValidation (pure <<< pure)

instance profunctorCoproductValidation ∷ (Monad m, Functor (s e)) ⇒ Profunctor (CoproductValidation m s e) where
  dimap f g (CoproductValidation v) =
    -- we have to map over `Either`
    CoproductValidation $ (f >>> pure) >=> v >=> (map g >>> pure)

instance choiceCoproductValidation ∷ (Monad m, Applicative (s e)) ⇒ Choice (CoproductValidation m s e) where
  left (CoproductValidation v) =
    CoproductValidation v'
   where
    v' (Right r) = pure (pure (Right r))
    v' (Left input) = (Left <$> _) <$> v input
  right (CoproductValidation v) =
    CoproductValidation v'
   where
    v' (Left r) = pure (pure (Left r))
    v' (Right input) = (Right <$> _) <$> (v input)

instance strongCoproductValidation ∷ (Monad m, Functor (s e)) ⇒ Strong (CoproductValidation m s e) where
  first (CoproductValidation v) =
    CoproductValidation v'
   where
    v' (Tuple input c) = ((flip Tuple c) <$> _) <$> (v input)
  second (CoproductValidation v) =
    CoproductValidation v'
   where
    v' (Tuple c input) = ((Tuple c) <$> _) <$> (v input)


-- | Beside these constructors you can also use just Applicative `pure`
validation ∷ ∀ a b e m s. (a → m (s e b)) → CoproductValidation m s e a b
validation = CoproductValidation

validation' ∷ forall a b e m p r r' s
  . RowCons p e r r'
  ⇒ Bifunctor s
  ⇒ Monad m
  ⇒ IsSymbol p
  ⇒ SProxy p
  → (a → m (s e b))
  → CoproductValidation m s (Variant r') a b
validation' l f = tag l (validation f)

pureV ∷ ∀ a b e m s. Monad m ⇒ Applicative (s e) ⇒ (a → s e b) → CoproductValidation m s e a b
pureV f = CoproductValidation (f >>> pure)

pureV' ∷ forall a b e m p r r' s
  . RowCons p e r r'
  ⇒ Applicative (s e)
  ⇒ Bifunctor s
  ⇒ Monad m
  ⇒ IsSymbol p
  ⇒ SProxy p
  → (a → s e b)
  → CoproductValidation m s (Variant r') a b
pureV' l f = tag l (pureV f)

fail ∷ ∀ a b e m s. (Monad m) ⇒ (MonadThrow e (s e)) ⇒ e → CoproductValidation m s e a b
fail = CoproductValidation <<< const <<< pure <<< throwError

fail' ∷ ∀ a b e m p r r' s
  . RowCons p e r r'
  ⇒ Applicative (s e)
  ⇒ Bifunctor s
  ⇒ MonadThrow e (s e)
  ⇒ Monad m
  ⇒ IsSymbol p
  ⇒ SProxy p
  → e
  → CoproductValidation m s (Variant r') a b
fail' p = tag p <<< CoproductValidation <<< const <<< pure <<< throwError

tag :: forall a b e m p r r' s
  . RowCons p e r r'
  ⇒ Bifunctor s
  ⇒ Monad m
  ⇒ IsSymbol p
  ⇒ SProxy p
  → CoproductValidation m s e a b
  → CoproductValidation m s (Variant r') a b
tag p (CoproductValidation v) =
  CoproductValidation ((lmap (inj p) <$> _) <$> v)

check :: forall a m s
  . Monad m
  ⇒ Applicative (s a)
  ⇒ MonadThrow a (s a)
  ⇒ (a → Boolean)
  → CoproductValidation m s a a a
check c = (pureV $ \a →
  if c a
    then (pure a)
    else (throwError a))

check' :: forall a l m r r' s
  . RowCons l a r r'
  ⇒ Applicative (s a)
  ⇒ Bifunctor s
  ⇒ MonadThrow a (s a)
  ⇒ Monad m
  ⇒ IsSymbol l
  ⇒ SProxy l
  → (a → Boolean)
  → CoproductValidation m s (Variant r') a a
check' t = check >>> tag t

