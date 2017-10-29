module Data.Validation.Jaws.Validation where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Either (Either(Left, Right))
import Data.EitherR (fmapLT)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant, inj)

-- | I is just (a → m (Either e b)) but those transformers
-- | give us so many things for free...
newtype Validation m e a b = Validation (ReaderT a (ExceptT e m) b)
derive instance functorValidation ∷ (Functor m) ⇒ Functor (Validation m e a)
derive newtype instance applyValidation ∷ (Monad m) ⇒ Apply (Validation m e a)
derive newtype instance applicativeValidation ∷ (Monad m) ⇒ Applicative (Validation m e a)
derive newtype instance bindValidation ∷ (Monad m) ⇒ Bind (Validation m e a)
derive newtype instance monadValidation ∷ (Monad m) ⇒ Monad (Validation m e a)

-- XXX: How to sum errors without API cluttering
-- and monoid requirement by default
instance altValidation ∷ (Monad m) ⇒ Alt (Validation m e a) where
  alt v1 v2 = Validation <<< ReaderT $ (\a → ExceptT $ do
    eb <- runValidation v1 a
    case eb of
      r@(Right b) → pure r
      _ → runValidation v2 a)

instance semigroupoidValidation ∷ (Monad m) ⇒ Semigroupoid (Validation m e) where
  compose (Validation v2) (Validation v1) =
    Validation (ReaderT (runReaderT v1 >=> runReaderT v2))

instance categoryValidation ∷ (Monad m) ⇒ Category (Validation m e) where
  id = Validation (ReaderT pure)

instance profunctorValidation ∷ (Monad m) ⇒ Profunctor (Validation m e) where
  dimap f g (Validation v) =
    Validation (ReaderT ((f >>> pure) >=> runReaderT v >=> (g >>> pure)))

instance choiceValidation ∷ (Monad m) ⇒ Choice (Validation m e) where
  left (Validation v) =
    Validation (ReaderT v')
   where
    v' (Left input) = Left <$> (runReaderT v input)
    v' (Right r) = pure (Right r)
  right (Validation v) =
    Validation (ReaderT v')
   where
    v' (Left l) = pure (Left l)
    v' (Right input) = Right <$> (runReaderT v input)

-- | Beside these four constructors you can also use just Applicative `pure`
validation ∷ ∀ a b e m. (a → m (Either e b)) → Validation m e a b
validation f = Validation <<< ReaderT $ (\a → ExceptT (f a))

validation' ∷ forall a b e m p r r'
  . RowCons p e r r'
  ⇒ Monad m
  ⇒ IsSymbol p
  ⇒ SProxy p
  → (a → m (Either e b))
  → Validation m (Variant r') a b
validation' l f = tag l (validation f)

pureV ∷ ∀ a b e. (a → Either e b) → ∀ m. (Monad m) ⇒ Validation m e a b
pureV f = Validation (ReaderT (f >>> pure >>> ExceptT))

pureV' ∷ forall a b e m p r r'
  . RowCons p e r r'
  ⇒ Monad m
  ⇒ IsSymbol p
  ⇒ SProxy p
  → (a → Either e b)
  → Validation m (Variant r') a b
pureV' l f = tag l (pureV f)

runValidation ∷ ∀ a b e m. Validation m e a b → a → m (Either e b)
runValidation (Validation v) = runExceptT <$> runReaderT v

tag :: forall a b e m p r r'
  . RowCons p e r r'
  ⇒ Monad m
  ⇒ IsSymbol p
  ⇒ SProxy p
  → Validation m e a b
  → Validation m (Variant r') a b
tag p (Validation v) =
  Validation (ReaderT $ fmapLT (inj p) <$> runReaderT v)

check :: forall a m
  . (Monad m)
  ⇒ (a → Boolean)
  → Validation m a a a
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
  → Validation m (Variant r') a a
check' t = check >>> tag t
