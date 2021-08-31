{-# LANGUAGE RankNTypes #-}
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
, atraverseOf
  -- * Unpacked
, UnpackedAffineTraversal(..)
, unpackedAffineTraversal
) where

import Fresnel.Profunctor.Optical
import Fresnel.Optic
import Data.Profunctor
import Data.Bifunctor
import Fresnel.Profunctor.AffineStar

-- Affine traversals

type AffineTraversal s t a b = forall p . AffineTraversing p => Optic p s t a b


-- Construction

atraversal :: (s -> Either t a) -> (s -> b -> t) -> AffineTraversal s t a b
atraversal prj set = dimap
  (\ s -> (prj s, set s))
  (\ (e, f) -> either id f e)
  . first' . right'


-- Elimination

matching :: AffineTraversal s t a b -> (s -> Either t a)
matching o = withAffineTraversal o const

withAffineTraversal :: AffineTraversal s t a b -> (((s -> Either t a) -> (s -> b -> t) -> r) -> r)
withAffineTraversal o = withUnpackedAffineTraversal (o (unpackedAffineTraversal Right (const id)))

atraverseOf :: Functor f => AffineTraversal s t a b -> (forall r . r -> f r) -> (a -> f b) -> (s -> f t)
atraverseOf o point = runAffineStar . o . affineStar point


-- Unpacked

newtype UnpackedAffineTraversal a b s t = UnpackedAffineTraversal { withUnpackedAffineTraversal :: forall r . ((s -> Either t a) -> (s -> b -> t) -> r) -> r }

instance Profunctor (UnpackedAffineTraversal a b) where
  dimap f g (UnpackedAffineTraversal r) = r $ \ prj set -> unpackedAffineTraversal (either (Left . g) Right . prj . f) (rmap g . set . f)

instance Strong (UnpackedAffineTraversal a b) where
  first'  (UnpackedAffineTraversal r) = r $ \ prj set -> unpackedAffineTraversal (\ (a, c) -> first (,c) (prj a)) (\ (a, c) b -> (set a b, c))
  second' (UnpackedAffineTraversal r) = r $ \ prj set -> unpackedAffineTraversal (\ (c, a) -> first (c,) (prj a)) (\ (c, a) b -> (c, set a b))

instance Choice (UnpackedAffineTraversal a b) where
  left' (UnpackedAffineTraversal r) = r $ \ prj set -> unpackedAffineTraversal (either (either (Left . Left) Right . prj) (Left . Right)) (\ e b -> first (`set` b) e)
  right' (UnpackedAffineTraversal r) = r $ \ prj set -> unpackedAffineTraversal (either (Left . Left) (either (Left . Right) Right . prj)) (\ e b -> fmap (`set` b) e)

instance IsIso (UnpackedAffineTraversal a b)
instance IsLens (UnpackedAffineTraversal a b)
instance IsPrism (UnpackedAffineTraversal a b)
instance AffineTraversing (UnpackedAffineTraversal a b)


unpackedAffineTraversal :: (s -> Either t a) -> (s -> b -> t) -> UnpackedAffineTraversal a b s t
unpackedAffineTraversal prj set = UnpackedAffineTraversal (\ k -> k prj set)
