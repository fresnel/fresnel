module Fresnel.Bifunctor.Contravariant
( Bicontravariant(..)
) where

class Bicontravariant p where
  contrabimap :: (a' -> a) -> (b' -> b) -> p a b -> p a' b'
