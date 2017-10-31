module Data.Validation.Jaws.Product where

import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Record (insert)
import Data.Symbol (SProxy)
import Data.Tuple (Tuple(..))
-- import Data.Validation.Jaws.Record (RecordBuilder)
-- import Data.Validation.Jaws.Validation (Validation(..), runValidation, tag)
import Type.Prelude (class IsSymbol, class RowLacks)

-- | This is really general type which is specialized below.
-- | I'm thinking of it as a builder which extracts/validates
-- | product elements from given `tok` and builds final value "attaching" them to
-- | result from previous validation steps.
-- | It is really close to categorical view of product - if we want to
-- | treat input data `tok` as product we have to be able to provide
-- | morphisms which extract elements of this product from it.
-- | As with CoproductValidation we don't want to use
-- | `ReaderT` here because it can be used in user application monad
-- | stack.
newtype Builder m tok a b = Builder (tok → a → m b)
derive instance functorBuilder ∷ (Functor m) ⇒ Functor (Builder m tok a)

instance applyBuilder ∷ (Monad m) ⇒ Apply (Builder m tok a) where
  apply bf ba = do
    f ← bf
    a ← ba
    pure $ f a

instance applicativeBuilder ∷ (Monad m) ⇒ Applicative (Builder m tok a) where
  pure = Builder <<< const <<< const <<< pure

instance bindBuilder ∷ (Monad m) ⇒ Bind (Builder m tok a) where
  bind (Builder v) f =
    Builder v'
   where
    v' tok a = do
      b ← v tok a
      let Builder r = f b
      r tok a

instance categoryBuilder ∷ (Monad m) ⇒ Category (Builder m tok) where
  id = Builder (\_ a → pure a)

instance semigroupoidBuilder ∷ (Bind m) ⇒ Semigroupoid (Builder m tok) where
  compose (Builder v1) (Builder v2) =
    Builder (\tok → v2 tok >=> v1 tok)

instance profunctorBuilder ∷ (Monad m) ⇒ Profunctor (Builder m tok) where
  dimap f g (Builder v) =
    Builder (\tok → f >>> v tok >>> (g <$> _))

instance choiceBuilder ∷ (Monad m) ⇒ Choice (Builder m tok) where
  left (Builder v) =
    Builder v'
   where
    v' tok (Left input) = Left <$> (v tok input)
    v' _ (Right r) = pure (Right r)
  right (Builder v) =
    Builder v'
   where
    v' tok (Right input) = Right <$> (v tok input)
    v' _ (Left l) = pure (Left l)

instance strongBuilder ∷ (Monad m) ⇒ Strong (Builder m a) where
  first (Builder v) =
    Builder v'
   where
    v' tok (Tuple input c) = flip Tuple c <$> (v tok input)
  second (Builder v) =
    Builder v'
   where
    v' tok (Tuple c input) = Tuple c <$> (v tok input)

-- | This is our helper type which traces both (valid and invalid)
-- | values on it's "success branch", so if any further validation
-- | failure we can easily build error value.

data Result i v =
  Invalid i | Valid i v

valid ∷ ∀ i v. v → Result (Either i v) v
valid v = Valid (Right v) v

instance functorResult ∷ Functor (Result i) where
  map f (Invalid i) = Invalid i
  map f (Valid i v) = Valid i (f v)

mapI ∷ ∀ i i' v. (i → i') → Result i v → Result i' v
mapI f (Invalid i) = Invalid (f i)
mapI f (Valid i v) = Valid (f i) v

instance bifunctorResult ∷ Bifunctor Result where
  bimap g f r = map f (mapI g r)


-- XXX: Fix this constraint and use them in product validation
-- class ProductChain a v v' | a v → v', v' v → a
-- 
-- instance _aProductChainTupleUnitUnit ∷ ProductChain Unit Unit Unit
-- 
-- instance _bProductChainTupleUnit ∷ (ProductChain v' Unit v') ⇒ ProductChain Unit (Tuple v v') (Tuple v v')
-- instance _cProductChainTupleRec ∷ (ProductChain v v' v'') ⇒ ProductChain (Tuple a v) v' (Tuple a v'')
-- 
-- instance _aProductChainRecordUnit ∷ ProductChain Unit (Record v) (Record v)
-- instance _bProductChainRecordRec
--   ∷ (IsSymbol l, RowCons l a br r, RowCons l a v' v'', ProductChain (Record br) (Record v) (Record v'))
--   ⇒ ProductChain (Record r) (Record v) (Record v'')

type ProductValidation m tok i i' v v' =
  (-- (ProductChain Unit i i')
   -- ⇒ (ProductChain Unit v v')
  Builder m tok (Result i v) (Result i' v'))

tupleValidation ∷ ∀ a b e i l m tok v
   . (Monad m)
   ⇒ (tok → m (Either e b))
   → ProductValidation m tok i (Tuple (Either e b) i) v (Tuple b v)
tupleValidation f =
  Builder (\tok p → toRecord p <$> (f tok))
 where
  toRecord (Valid ir vr) r@(Right b) = Valid (Tuple r ir) (Tuple b vr)
  toRecord (Valid ir _) e = Invalid (Tuple e ir)
  toRecord (Invalid ir) e = Invalid (Tuple e ir)

pureTupleValidation ∷ ∀ a b e i l m tok v
   . (Monad m)
   ⇒ (tok → Either e b)
   → ProductValidation m tok i (Tuple (Either e b) i) v (Tuple b v)
pureTupleValidation f =
  tupleValidation (f >>> pure)

pureRecordFieldValidation ∷ ∀ a b e i i' l m tok v v'
  . (IsSymbol l)
  ⇒ (Monad m)
  ⇒ (RowCons l (Either e b) i i')
  ⇒ (RowCons l b v v')
  ⇒ (RowLacks l i)
  ⇒ (RowLacks l v)
  ⇒ (Functor m)
  ⇒ SProxy l
  → (tok → Either e b)
  → ProductValidation m tok (Record i) (Record i') (Record v) (Record v')
pureRecordFieldValidation p v =
  recordFieldValidation p (v >>> pure)

recordFieldValidation ∷ ∀ a b e i i' l m tok v v'
  . (IsSymbol l)
  ⇒ (Monad m)
  ⇒ (RowCons l (Either e b) i i')
  ⇒ (RowCons l b v v')
  ⇒ (RowLacks l i)
  ⇒ (RowLacks l v)
  ⇒ (Functor m)
  ⇒ SProxy l
  → (tok → m (Either e b))
  → ProductValidation m tok (Record i) (Record i') (Record v) (Record v')
recordFieldValidation p v =
  Builder (\tok p → toRecord p <$> (v tok))
 where
 toRecord (Valid ir vr) r@(Right b) = Valid (insert p r ir) (insert p b vr)
 toRecord (Valid ir _) e = Invalid (insert p e ir)
 toRecord (Invalid ir) e = Invalid (insert p e ir)

runRecordValidation ∷ ∀ a e i i' m n n' tok v v'
  . Monad m
  ⇒ ProductValidation m tok {} (Record i) {} (Record v)
  → (tok → m (Either (Record i) (Record v)))
runRecordValidation (Builder p) =
  (\tok → do
    r ← p tok (Valid {} {})
    case r of
      Invalid i → pure (Left i)
      Valid _ v → pure (Right v))

