{-# LANGUAGE RankNTypes #-}
module Fresnel.Semigroup.Snoc1
( -- * Non-empty snoc lists
  Snoc1(..)
) where

-- Non-empty snoc lists

newtype Snoc1 a = Snoc1 { runSnoc1 :: forall r . (a -> r) -> (r -> a -> r) -> r }
