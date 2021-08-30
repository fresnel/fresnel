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
, atraverseOf
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
  first' (UnpackedAffineTraversal r) = r $ \ prj set -> unpackedAffineTraversal (\ (a, c) -> first (,c) (prj a)) (\ (a, c) b -> (set a b, c))

instance Choice (UnpackedAffineTraversal a b) where
  left' (UnpackedAffineTraversal r) = r $ \ prj set -> unpackedAffineTraversal (either (either (Left . Left) Right . prj) (Left . Right)) (\ e b -> first (`set` b) e)

instance Isoing (UnpackedAffineTraversal a b)
instance Lensing (UnpackedAffineTraversal a b)
instance Prisming (UnpackedAffineTraversal a b)
instance AffineTraversing (UnpackedAffineTraversal a b)


unpackedAffineTraversal :: (s -> Either t a) -> (s -> b -> t) -> UnpackedAffineTraversal a b s t
unpackedAffineTraversal prj set = UnpackedAffineTraversal (\ k -> k prj set)


newtype AffineStar f a b = AffineStar { withAffineStar :: forall r . ((forall x . x -> f x) -> (a -> f b) -> r) -> r }

instance Functor f => Profunctor (AffineStar f) where
  dimap f g = mapAffineStar (dimap f (fmap g))

instance Functor f => Choice (AffineStar f) where
  left' (AffineStar r) = AffineStar (\ k -> r (\ point f -> k point (either (fmap Left . f) (fmap Right . point))))

instance Functor f => Strong (AffineStar f) where
  first'  r = mapAffineStar (\ f (a, c) -> (,c) <$> f a) r
  second' r = mapAffineStar (\ f (c, a) -> (c,) <$> f a) r

instance Functor f => Isoing (AffineStar f)
instance Functor f => Lensing (AffineStar f)
instance Functor f => Prisming (AffineStar f)
instance Functor f => AffineTraversing (AffineStar f)

affineStar :: (forall x . x -> f x) -> (a -> f b) -> AffineStar f a b
affineStar point f = AffineStar (\ k -> k point f)

runAffineStar :: AffineStar f a b -> (a -> f b)
runAffineStar a = withAffineStar a (\ _ f -> f)

mapAffineStar :: ((a -> f b) -> (c -> f d)) -> (AffineStar f a b -> AffineStar f c d)
mapAffineStar f (AffineStar r) = AffineStar (\ k -> r (\ point -> k point . f))
