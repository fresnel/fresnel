{-# LANGUAGE RankNTypes #-}
module Fresnel.Fold
( -- Folds
  Fold
) where

import Data.Profunctor
import Data.Profunctor.Traversing
import Fresnel.Bifunctor.Contravariant
import Fresnel.Optic

-- Folds

type Fold s a = forall p . (Bicontravariant p, Cochoice p, Traversing p) => Optic' p s a
