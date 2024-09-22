{-# LANGUAGE RankNTypes #-}
module Fresnel.Semigroup.Fork1
( -- * Non-empty binary trees
  Fork1(..)
) where

import Data.Foldable (toList)
import Data.Foldable1

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
