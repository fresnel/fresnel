module Fresnel.Bifunctor.Contravariant
( -- * Bicontravariant functors
  Bicontravariant(..)
) where

import Data.Profunctor (Forget(..), Profunctor(..))

-- Bicontravariant functors

class Bicontravariant p where
  contrabimap :: (a' -> a) -> (b' -> b) -> p a b -> p a' b'

instance Bicontravariant (Forget r) where
  contrabimap f _ = Forget . lmap f . runForget
