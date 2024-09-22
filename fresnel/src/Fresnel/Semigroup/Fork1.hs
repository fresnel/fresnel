{-# LANGUAGE RankNTypes #-}
module Fresnel.Semigroup.Fork1
( -- * Non-empty binary trees
  Fork1(..)
  -- * Construction
, singleton
) where

import Data.Foldable (toList)
import Data.Foldable1
import Data.Functor.Apply

-- Non-empty binary trees

newtype Fork1 a = Fork1 { runFork1 :: forall r . (r -> r -> r) -> (a -> r) -> r }

instance Show a => Show (Fork1 a) where
  showsPrec _ = showList . toList

instance Semigroup (Fork1 a) where
  Fork1 a1 <> Fork1 a2 = Fork1 (\ (<>) singleton -> a1 (<>) singleton <> a2 (<>) singleton)

instance Foldable Fork1 where
  foldMap = foldMap1

instance Foldable1 Fork1 where
  foldMap1 f (Fork1 r) = r (<>) f

instance Functor Fork1 where
  fmap f (Fork1 r) = Fork1 (\ (<>) singleton -> r (<>) (singleton . f))

instance Traversable Fork1 where
  traverse f (Fork1 r) = r (liftA2 (<>)) (fmap singleton . f)

instance Apply Fork1 where
  liftF2 f (Fork1 a) (Fork1 b) = Fork1 (\ (<>) singleton -> a (<>) (\ a' -> b (<>) (singleton . f a')))

instance Applicative Fork1 where
  pure = singleton

  liftA2 = liftF2


-- Construction

singleton :: a -> Fork1 a
singleton a = Fork1 (\ _ singleton -> singleton a)
