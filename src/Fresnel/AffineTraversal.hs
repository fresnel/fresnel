{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Fresnel.AffineTraversal
( -- * Affine traversals
  AffineTraversal
, AffineTraversing
  -- * Construction
, atraversal
  -- * Unpacked
, UnpackedAffineTraversal(..)
, unpackedAffineTraversal
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

unpackedAffineTraversal :: (s -> Either t a) -> (s -> b -> t) -> UnpackedAffineTraversal a b s t
unpackedAffineTraversal prj set = UnpackedAffineTraversal (\ k -> k prj set)
