{-# LANGUAGE RankNTypes #-}
module Fresnel.Monoid.Cons
( -- * Cons lists
  Cons(..)
  -- * Construction
, singleton
, cons
, nil
) where

-- Cons lists

newtype Cons a = Cons { runCons :: forall r . (a -> r -> r) -> r -> r }

instance Semigroup (Cons a) where
  Cons a1 <> Cons a2 = Cons (\ cons -> a1 cons . a2 cons)

instance Monoid (Cons a) where
  mempty = Cons (\ _ nil -> nil)

instance Foldable Cons where
  foldMap f (Cons r) = r (mappend . f) mempty
  foldr f z (Cons r) = r f z


-- Construction

singleton :: a -> Cons a
singleton a = Cons (\ cons nil -> cons a nil)

cons :: a -> Cons a -> Cons a
cons a (Cons as) = Cons (\ cons -> cons a . as cons)

nil :: Cons a
nil = Cons (\ _ nil -> nil)
