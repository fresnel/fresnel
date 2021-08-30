{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Fresnel.Profunctor.AffineStar
( -- * Affine star profunctors
  AffineStar(..)
  -- * Construction
, affineStar
  -- * Elimination
, runAffineStar
  -- * Computation
, mapAffineStar
) where

import Data.Profunctor
import Fresnel.Profunctor.Optical

-- Affine star profunctors

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


-- Construction

affineStar :: (forall x . x -> f x) -> (a -> f b) -> AffineStar f a b
affineStar point f = AffineStar (\ k -> k point f)


-- Elimination

runAffineStar :: AffineStar f a b -> (a -> f b)
runAffineStar a = withAffineStar a (\ _ f -> f)


-- Computation

mapAffineStar :: ((a -> f b) -> (c -> f d)) -> (AffineStar f a b -> AffineStar f c d)
mapAffineStar f (AffineStar r) = AffineStar (\ k -> r (\ point -> k point . f))
