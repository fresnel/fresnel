{-# LANGUAGE RankNTypes #-}
module Fresnel.Lens
( -- * Lenses
  Lens
, Lens'
  -- * Construction
, lens
  -- * Elimination
, withLens
  -- * Tuples
, fst_
, snd_
, alongside
  -- * Unpacked
, UnpackedLens(..)
, unpackedLens
) where

import Control.Arrow ((&&&), (***))
import Data.Profunctor
import Fresnel.Optic

-- Lenses

type Lens s t a b = forall p . Strong p => Optic p s t a b

type Lens' s a = Lens s s a a


-- Construction

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = dimap (id &&& get) (uncurry set) . second'


-- Elimination

withLens :: Lens s t a b -> (((s -> a) -> (s -> b -> t) -> r) -> r)
withLens o = withUnpackedLens (o (unpackedLens id (const id)))


-- Tuples

fst_ :: Lens (a, b) (a', b) a a'
fst_ = lens fst (\ s a' -> (a', snd s))

snd_ :: Lens (a, b) (a, b') b b'
snd_ = lens snd (\ s b' -> (fst s, b'))

alongside :: Lens s1 t1 a1 b1 -> Lens s2 t2 a2 b2 -> Lens (s1, s2) (t1, t2) (a1, a2) (b1, b2)
alongside o1 o2 = withLens o1 $ \ get1 set1 -> withLens o2 $ \ get2 set2 ->
  lens (get1 *** get2) (uncurry (***) . (set1 *** set2))


-- Unpacked

-- | A 'Lens' unpacked into the get & set functions it was constructed from.
newtype UnpackedLens a b s t = UnpackedLens { withUnpackedLens :: forall r . ((s -> a) -> (s -> b -> t) -> r) -> r }

instance Profunctor (UnpackedLens a b) where
  dimap f g (UnpackedLens r) = r $ \ get set -> unpackedLens (get . f) (rmap g . set . f)

instance Strong (UnpackedLens a b) where
  first' (UnpackedLens r) = r $ \ get set -> unpackedLens (get . fst) (\ (a, c) b -> (set a b, c))


unpackedLens :: (s -> a) -> (s -> b -> t) -> UnpackedLens a b s t
unpackedLens get set = UnpackedLens (\ k -> k get set)
