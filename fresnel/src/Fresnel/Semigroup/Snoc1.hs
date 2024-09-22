{-# LANGUAGE RankNTypes #-}
module Fresnel.Semigroup.Snoc1
( -- * Non-empty snoc lists
  Snoc1(..)
) where

-- Non-empty snoc lists

newtype Snoc1 a = Snoc1 { runSnoc1 :: forall r . (a -> r) -> (r -> a -> r) -> r }

instance Foldable Snoc1 where
  foldMap f (Snoc1 r) = r f ((. f) . (<>))
  foldl f z (Snoc1 r) = r (z `f`) f
