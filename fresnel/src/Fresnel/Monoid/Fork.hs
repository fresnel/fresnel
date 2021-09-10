module Fresnel.Monoid.Fork
( Fork(..)
) where

newtype Fork r a = Fork { runFork :: (r -> r -> r) -> (a -> r) -> r -> r }
