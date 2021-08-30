{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Fresnel.AffineTraversal
( -- * Affine traversals
  AffineTraversal
, AffineTraversing
  -- * Construction
, atraversal
  -- * Elimination
, matching
, withAffineTraversal
  -- * Unpacked
, UnpackedAffineTraversal(..)
, unpackedAffineTraversal
, UnpackedProject(..)
, unpackedProject
) where

import Fresnel.Profunctor.Optical
import Fresnel.Optic
import Data.Profunctor
import Data.Bifunctor

-- Affine traversals

type AffineTraversal s t a b = forall p . AffineTraversing p => Optic p s t a b


-- Construction

atraversal :: (s -> Either t a) -> (s -> b -> t) -> AffineTraversal s t a b
atraversal prj set = dimap
  (\ s -> (prj s, set s))
  (\case
    (Left  t, _) -> t
    (Right b, f) -> f b)
  . first' . right'


-- Elimination

matching :: AffineTraversal s t a b -> (s -> Either t a)
matching o = withUnpackedProject (o (unpackedProject Right)) id

withAffineTraversal :: AffineTraversal s t a b -> (((s -> Either t a) -> (s -> b -> t) -> r) -> r)
withAffineTraversal o = withUnpackedAffineTraversal (o (unpackedAffineTraversal Right (const id)))


-- Unpacked

newtype UnpackedAffineTraversal a b s t = UnpackedAffineTraversal { withUnpackedAffineTraversal :: forall r . ((s -> Either t a) -> (s -> b -> t) -> r) -> r }

instance Profunctor (UnpackedAffineTraversal a b) where
  dimap f g (UnpackedAffineTraversal r) = r $ \ prj set -> unpackedAffineTraversal (either (Left . g) Right . prj . f) (rmap g . set . f)

instance Strong (UnpackedAffineTraversal a b) where
  first' (UnpackedAffineTraversal r) = r $ \ prj set -> unpackedAffineTraversal (\ (a, c) -> first (,c) (prj a)) (\ (a, c) b -> (set a b, c))

instance Choice (UnpackedAffineTraversal a b) where
  left' (UnpackedAffineTraversal r) = r $ \ prj set -> unpackedAffineTraversal (either (either (Left . Left) Right . prj) (Left . Right)) (\ e b -> first (`set` b) e)

instance Isoing (UnpackedAffineTraversal a b)
instance Lensing (UnpackedAffineTraversal a b)
instance Prisming (UnpackedAffineTraversal a b)
instance AffineTraversing (UnpackedAffineTraversal a b)


unpackedAffineTraversal :: (s -> Either t a) -> (s -> b -> t) -> UnpackedAffineTraversal a b s t
unpackedAffineTraversal prj set = UnpackedAffineTraversal (\ k -> k prj set)


newtype UnpackedProject a s t = UnpackedProject { withUnpackedProject :: forall r . ((s -> Either t a) -> r) -> r }

instance Profunctor (UnpackedProject a) where
  dimap f g (UnpackedProject r) = r $ \ prj -> unpackedProject (either (Left . g) Right . prj . f)

instance Strong (UnpackedProject a) where
  first' (UnpackedProject r) = r $ \ prj -> unpackedProject (\ (a, c) -> first (,c) (prj a))

instance Choice (UnpackedProject a) where
  left' (UnpackedProject r) = r $ \ prj -> unpackedProject (either (either (Left . Left) Right . prj) (Left . Right))

instance Isoing (UnpackedProject a)
instance Lensing (UnpackedProject a)
instance Prisming (UnpackedProject a)
instance AffineTraversing (UnpackedProject a)


unpackedProject :: (s -> Either t a) -> UnpackedProject a s t
unpackedProject prj = UnpackedProject (\ k -> k prj)
