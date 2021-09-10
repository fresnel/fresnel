module Fresnel.Monoid.Cons
( cons
, Cons(..)
) where

cons :: a -> Cons r a
cons a = Cons (\ cons nil -> cons a nil)

newtype Cons r a = Cons { runCons :: (a -> r -> r) -> r -> r }

instance Semigroup (Cons r a) where
  Cons a1 <> Cons a2 = Cons (\ cons -> a1 cons . a2 cons)

instance Monoid (Cons r a) where
  mempty = Cons (\ _ nil -> nil)
