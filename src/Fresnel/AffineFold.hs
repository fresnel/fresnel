{-# LANGUAGE RankNTypes #-}
module Fresnel.AffineFold
( -- * Affine folds
  AffineFold
  -- * Construction
, afolding
) where

import Data.Profunctor
import Data.Profunctor.Traversing
import Fresnel.Bifunctor.Contravariant
import Fresnel.Optic

-- Affine folds

-- FIXME: this is stronger than we actually mean; we need a Visiting class to express that.
type AffineFold s a = forall p . (Bicontravariant p, Traversing p) => Optic' p s a


-- Construction

afolding :: (s -> Maybe a) -> AffineFold s a
afolding f = contrabimap ((`maybe` Right) . Left <*> f) Left . right'
