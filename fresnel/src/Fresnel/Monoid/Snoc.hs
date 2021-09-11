{-# LANGUAGE RankNTypes #-}
module Fresnel.Monoid.Snoc
( -- * Snoc lists
  Snoc(..)
  -- * Construction
, singleton
) where

-- Snoc lists

newtype Snoc a = Snoc { runSnoc :: forall r . (r -> a -> r) -> r -> r }


-- Construction

singleton :: a -> Snoc a
singleton a = Snoc (\ snoc nil -> snoc nil a)
