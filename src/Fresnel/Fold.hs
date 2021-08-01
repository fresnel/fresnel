{-# LANGUAGE RankNTypes #-}
module Fresnel.Fold
( -- * Folds
  Fold
  -- * Construction
, folded
  -- * Elimination
, foldMapOf
, foldOf
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


-- Elimination

foldMapOf :: Monoid m => Fold s a -> (a -> m) -> (s -> m)
foldMapOf o = runForget . o . Forget

foldOf :: Monoid a => Fold s a -> (s -> a)
foldOf o = runForget (o (Forget id))
