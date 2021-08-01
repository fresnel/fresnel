module Fresnel.Bifunctor.Contravariant
( -- * Bicontravariant functors
  Bicontravariant(..)
) where

-- Bicontravariant functors

class Bicontravariant p where
  contrabimap :: (a' -> a) -> (b' -> b) -> p a b -> p a' b'
