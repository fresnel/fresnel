module Fresnel.Bifunctor.Contravariant
( -- * Bicontravariant functors
  Bicontravariant(..)
, contrafirst
, contrasecond
  -- * Phantom parameters
, rphantom
, biphantom
) where

import Data.Bifunctor (Bifunctor(..))
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


-- Phantom parameters

rphantom :: (Profunctor p, Bicontravariant p) => p a b -> p a c
rphantom = contrasecond (const ()) . rmap (const ())


biphantom :: (Bifunctor p, Bicontravariant p) => p a b -> p c d
biphantom = contrabimap (const ()) (const ()) . bimap (const ()) (const ())
