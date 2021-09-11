{-# LANGUAGE RankNTypes #-}
module Fresnel.Monoid.Snoc
( -- * Snoc lists
  Snoc(..)
) where

-- Snoc lists

newtype Snoc a = Snoc { runSnoc :: forall r . (r -> a -> r) -> r -> r }
