{-# LANGUAGE RankNTypes #-}
module Fresnel.Semigroup.Snoc1
( -- * Non-empty snoc lists
  Snoc1(..)
) where

import Data.Foldable1

-- Non-empty snoc lists

newtype Snoc1 a = Snoc1 { runSnoc1 :: forall r . (a -> r) -> (r -> a -> r) -> r }

instance Foldable Snoc1 where
  foldMap f (Snoc1 r) = r f ((. f) . (<>))
  foldl f z (Snoc1 r) = r (z `f`) f

instance Foldable1 Snoc1 where
  foldMap1 f (Snoc1 r) = r f ((. f) . (<>))
  foldlMap1 f g (Snoc1 r) = r f g

instance Functor Snoc1 where
  fmap h (Snoc1 r) = Snoc1 (\ f g -> r (f . h) ((. h) . g))
