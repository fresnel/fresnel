module Fresnel.Monoid.Fork
( singleton
, Fork(..)
) where

singleton :: a -> Fork r a
singleton a = Fork (\ _ leaf _ -> leaf a)

newtype Fork r a = Fork { runFork :: (r -> r -> r) -> (a -> r) -> r -> r }

instance Semigroup (Fork r a) where
  Fork a1 <> Fork a2 = Fork (\ fork leaf nil -> a1 fork leaf nil `fork` a2 fork leaf nil)

instance Monoid (Fork r a) where
  mempty = Fork (\ _ _ nil -> nil)
