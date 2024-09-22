{-# LANGUAGE RankNTypes #-}
module Fresnel.Fold1
( -- * Relevant folds
  Fold1
  -- * Construction
, folded1
, fold1ing
  -- * Elimination
, foldMap1Of
) where

import Data.Profunctor
import Data.Profunctor.Unsafe ((#.), (.#))
import Data.Semigroup.Foldable
import Fresnel.Bifunctor.Contravariant
import Fresnel.Fold1.Internal (IsFold1)
import Fresnel.Optic (Optic')
import Fresnel.Traversal1

-- Relevant folds

type Fold1 s a = forall p . IsFold1 p => Optic' p s a


-- Construction

folded1 :: Foldable1 t => Fold1 (t a) a
folded1 = rphantom . traversal1 traverse1_

fold1ing :: Foldable1 t => (s -> t a) -> Fold1 s a
fold1ing f = contrabimap f (const ()) . traversal1 traverse1_


-- Elimination

foldMap1Of :: Semigroup m => Fold1 s a -> ((a -> m) -> (s -> m))
foldMap1Of o = runForget #. o .# Forget
