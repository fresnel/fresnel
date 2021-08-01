{-# LANGUAGE RankNTypes #-}
module Fresnel.Fold
( -- * Folds
  Fold
  -- * Construction
, folded
) where

import Data.Foldable (traverse_)
import Data.Profunctor
import Data.Profunctor.Traversing
import Fresnel.Bifunctor.Contravariant
import Fresnel.Optic

-- Folds

type Fold s a = forall p . (Bicontravariant p, Cochoice p, Traversing p) => Optic' p s a


-- Construction

folded :: Foldable f => Fold (f a) a
folded = rphantom . wander traverse_
