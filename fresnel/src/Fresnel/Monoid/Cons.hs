module Fresnel.Monoid.Cons
( Cons(..)
) where

newtype Cons r a = Cons { runCons :: (a -> r -> r) -> r -> r }

instance Semigroup (Cons r a) where
  Cons a1 <> Cons a2 = Cons (\ cons -> a1 cons . a2 cons)
