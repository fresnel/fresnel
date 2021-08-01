module Fresnel.Bifunctor.Contravariant
( -- * Bicontravariant functors
  Bicontravariant(..)
, contrafirst
, contrasecond
) where

import Data.Profunctor (Forget(..), Profunctor(..))

-- Bicontravariant functors

class Bicontravariant p where
  contrabimap :: (a' -> a) -> (b' -> b) -> p a b -> p a' b'

instance Bicontravariant (Forget r) where
  contrabimap f _ = Forget . lmap f . runForget


contrafirst :: Bicontravariant p => (a' -> a) -> p a b -> p a' b
contrafirst = (`contrabimap` id)

contrasecond :: Bicontravariant p => (b' -> b) -> p a b -> p a b'
contrasecond = (id `contrabimap`)
