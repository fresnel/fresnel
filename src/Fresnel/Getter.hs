{-# LANGUAGE RankNTypes #-}
module Fresnel.Getter
( -- * Getters
  Getter
  -- * Construction
, to
, getting
  -- * Elimination
, views
, view
, (^.)
  -- * Utilities
, Bicontravariant(..)
, contrafirst
, contrasecond
, rphantom
) where

import Data.Profunctor
import Fresnel.Bifunctor.Contravariant
import Fresnel.Optic

-- Getters

type Getter s a = forall p . (Bicontravariant p, Strong p) => Optic' p s a


-- Construction

to :: (s -> a) -> Getter s a
to f = lmap f . rphantom


getting :: (Profunctor p, Bicontravariant p) => Optic p s t a b -> Optic' p s a
getting l f = rphantom . l $ rphantom f


-- Elimination

views :: Getter s a -> (a -> r) -> (s -> r)
views b = runForget . b . Forget

view :: Getter s a -> (s -> a)
view b = views b id

(^.) :: s -> Getter s a -> a
s ^. o = view o s

infixl 8 ^.


-- Utilities

instance Bicontravariant (Forget r) where
  contrabimap f _ = Forget . lmap f . runForget

contrafirst :: Bicontravariant p => (a' -> a) -> p a b -> p a' b
contrafirst = (`contrabimap` id)

contrasecond :: Bicontravariant p => (b' -> b) -> p a b -> p a b'
contrasecond = (id `contrabimap`)


rphantom :: (Profunctor p, Bicontravariant p) => p a b -> p a c
rphantom = contrasecond (const ()) . rmap (const ())
