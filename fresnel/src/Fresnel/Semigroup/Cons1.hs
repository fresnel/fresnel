{-# LANGUAGE RankNTypes #-}
module Fresnel.Semigroup.Cons1
( -- * Non-empty cons lists
  Cons1(..)
) where

-- Non-empty cons lists

newtype Cons1 a = Cons1 { runCons1 :: forall r . (a -> r) -> (a -> r -> r) -> r }
