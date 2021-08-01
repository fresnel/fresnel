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
  -- * Unpacked
, UnpackedLens(..)
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


-- Elimination

withLens :: Lens s t a b -> (((s -> a) -> (s -> b -> t) -> r) -> r)
withLens o = withUnpackedLens (o (UnpackedLens (\ k -> k id (const id))))


-- Tuples

fst_ :: Lens (a, b) (a', b) a a'
fst_ = lens fst (\ s a' -> (a', snd s))

snd_ :: Lens (a, b) (a, b') b b'
snd_ = lens snd (\ s b' -> (fst s, b'))


-- Unpacked

-- | A 'Lens' unpacked into the get & set functions it was constructed from.
newtype UnpackedLens a b s t = UnpackedLens { withUnpackedLens :: forall r . ((s -> a) -> (s -> b -> t) -> r) -> r }

instance Profunctor (UnpackedLens a b) where
  dimap f g (UnpackedLens r) = r $ \ get set -> UnpackedLens $ \ k -> k (get . f) (rmap g . set . f)

instance Strong (UnpackedLens a b) where
  first' (UnpackedLens r) = r $ \ get set -> UnpackedLens $ \ k -> k (get . fst) (\ (a, c) b -> (set a b, c))
