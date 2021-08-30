{-# LANGUAGE RankNTypes #-}
module Fresnel.AffineTraversal
( -- * Affine traversals
  AffineTraversal
, AffineTraversing
) where

import Fresnel.Profunctor.Optical
import Fresnel.Optic

-- Affine traversals

type AffineTraversal s t a b = forall p . AffineTraversing p => Optic p s t a b
