{-# LANGUAGE RankNTypes #-}
module Fresnel.Fold1
( -- * Relevant folds
  Fold1
  -- * Construction
, folded1
, unfolded1
, fold1ing
, foldMap1ing
  -- * Elimination
, foldMap1Of
, foldMap1ByOf
) where

import Data.Functor (void)
import Data.Functor.Apply
import Data.Profunctor
import Data.Profunctor.Unsafe ((#.), (.#))
import Data.Semigroup.Foldable
import Fresnel.Bifunctor.Contravariant
import Fresnel.Fold1.Internal (IsFold1)
import Fresnel.Functor.Ap1
import Fresnel.Optic (Optic')
import Fresnel.Semigroup.Fork1
import Fresnel.Traversal1

-- Relevant folds

type Fold1 s a = forall p . IsFold1 p => Optic' p s a


-- Construction

folded1 :: Foldable1 t => Fold1 (t a) a
folded1 = rphantom . traversal1 traverse1_

unfolded1 :: (s -> (a, Maybe s)) -> Fold1 s a
unfolded1 coalg = rphantom . traversal1 (\ f -> let loop s = case coalg s of { (a, Nothing) -> f a ; (a, Just s') -> f a .> loop s' } in loop)

fold1ing :: Foldable1 t => (s -> t a) -> Fold1 s a
fold1ing f = contrabimap f (const ()) . traversal1 traverse1_

-- | Make a 'Fold1' by lifting a 'foldMap1'-like function.
foldMap1ing :: (forall m . Semigroup m => (a -> m) -> (s -> m)) -> Fold1 s a
foldMap1ing fm = rphantom . traversal1 (\ f -> getAp1 #. fm (Ap1 #. void . f))


-- Elimination

foldMap1Of :: Semigroup m => Fold1 s a -> ((a -> m) -> (s -> m))
foldMap1Of o = runForget #. o .# Forget

foldMap1ByOf :: Fold1 s a -> ((r -> r -> r) -> (a -> r) -> (s -> r))
foldMap1ByOf o fork leaf s = runFork1 (runForget (o (Forget singleton)) s) fork leaf
