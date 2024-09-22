{-# LANGUAGE RankNTypes #-}
module Fresnel.Semigroup.Fork1
( -- * Non-empty binary trees
  Fork1(..)
) where

-- Non-empty binary trees

newtype Fork1 a = Fork1 { runFork1 :: forall r . (r -> r -> r) -> (a -> r) -> r }

instance Foldable Fork1 where
  foldMap f (Fork1 r) = r (<>) f
