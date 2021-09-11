{-# LANGUAGE RankNTypes #-}
module Fresnel.Monoid.Cons
( -- * Cons lists
  Cons(..)
  -- * Construction
, singleton
, cons
, nil
) where

import Data.Foldable (toList)

-- Cons lists

newtype Cons a = Cons { runCons :: forall r . (a -> r -> r) -> r -> r }

instance Show a => Show (Cons a) where
  showsPrec _ = showList . toList

instance Semigroup (Cons a) where
  Cons a1 <> Cons a2 = Cons (\ cons -> a1 cons . a2 cons)

instance Monoid (Cons a) where
  mempty = nil

instance Foldable Cons where
  foldMap f (Cons r) = r (mappend . f) mempty
  foldr f z (Cons r) = r f z

instance Functor Cons where
  fmap f (Cons r) = Cons (\ cons -> r (cons . f))


-- Construction

singleton :: a -> Cons a
singleton a = Cons (\ cons nil -> cons a nil)

cons :: a -> Cons a -> Cons a
cons a (Cons as) = Cons (\ cons -> cons a . as cons)

nil :: Cons a
nil = Cons (\ _ nil -> nil)
