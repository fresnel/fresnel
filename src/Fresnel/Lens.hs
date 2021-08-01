{-# LANGUAGE RankNTypes #-}
module Fresnel.Lens
( -- * Lenses
  Lens
, Lens'
  -- * Construction
, lens
  -- * Tuples
, fst_
, snd_
  -- * Unpacked
, UnpackedLens(..)
, withUnpackedLens
) where

import Control.Arrow ((&&&))
import Data.Profunctor
import Fresnel.Optic

-- Lenses

type Lens s t a b = forall p . Strong p => Optic p s t a b

type Lens' s a = Lens s s a a


-- Construction

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = dimap (id &&& get) (uncurry set) . second'


-- Tuples

fst_ :: Lens (a, b) (a', b) a a'
fst_ = lens fst (\ s a' -> (a', snd s))

snd_ :: Lens (a, b) (a, b') b b'
snd_ = lens snd (\ s b' -> (fst s, b'))


-- Unpacked

-- | A 'Lens' unpacked into the get & set functions it was constructed from.
data UnpackedLens a b s t = UnpackedLens { get :: s -> a, set :: s -> b -> t }

instance Profunctor (UnpackedLens a b) where
  dimap f g (UnpackedLens get set) = UnpackedLens (get . f) (rmap g . set . f)

instance Strong (UnpackedLens a b) where
  first' (UnpackedLens get set) = UnpackedLens (get . fst) (\ (a, c) b -> (set a b, c))

withUnpackedLens :: UnpackedLens a b s t -> (((s -> a) -> (s -> b -> t) -> r) -> r)
withUnpackedLens (UnpackedLens get set) f = f get set
