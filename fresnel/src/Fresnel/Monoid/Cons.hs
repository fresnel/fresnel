module Fresnel.Monoid.Cons
( -- * Cons lists
  Cons(..)
  -- * Construction
, singleton
, cons
, nil
) where

-- Cons lists

newtype Cons r a = Cons { runCons :: (a -> r -> r) -> r -> r }

instance Semigroup (Cons r a) where
  Cons a1 <> Cons a2 = Cons (\ cons -> a1 cons . a2 cons)

instance Monoid (Cons r a) where
  mempty = Cons (\ _ nil -> nil)


-- Construction

singleton :: a -> Cons r a
singleton a = Cons (\ cons nil -> cons a nil)

cons :: a -> Cons r a -> Cons r a
cons a (Cons as) = Cons (\ cons -> cons a . as cons)

nil :: Cons r a
nil = Cons (\ _ nil -> nil)
