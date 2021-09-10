module Fresnel.Monoid.Cons
( Cons(..)
) where

newtype Cons r a = Cons { runCons :: (a -> r -> r) -> r -> r }
