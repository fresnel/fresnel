{-# LANGUAGE RankNTypes #-}
module Fresnel.Fold1
( -- * Relevant folds
  Fold1
, IsFold1
  -- * Construction
, folded1
, unfolded1
, fold1ing
, foldMap1ing
, iterated
, repeated
  -- * Elimination
, foldMap1Of
, foldMap1ByOf
, foldrMap1Of
, foldlMap1Of
, fold1Of
, fold1ByOf
, sequence1Of_
, traverse1Of_
, for1Of_
, toList1Of
, concatOf
, concatMapOf
, firstOf
, lastOf
, minimumOf
, minimumByOf
, maximumOf
, maximumByOf
  -- * Union semigroup
, Union(..)
) where

import Data.Functor (void)
import Data.Functor.Apply
import Data.List.NonEmpty (NonEmpty)
import Data.Profunctor
import Data.Profunctor.Unsafe ((#.), (.#))
import Data.Semigroup (First(..), Last(..))
import Data.Semigroup.Foldable
import Fresnel.Bifunctor.Contravariant
import Fresnel.Fold1.Internal (IsFold1)
import Fresnel.Functor.Ap1
import Fresnel.Functor.Traversed1
import Fresnel.Optic (Optic')
import Fresnel.Semigroup.Cons1 as Cons1
import Fresnel.Semigroup.Fork1 as Fork1
import Fresnel.Semigroup.Snoc1 as Snoc1
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

iterated :: (a -> a) -> Fold1 a a
iterated f = rphantom . traversal1 (\ g -> let loop a = g a .> loop (f a) in loop)

-- | An infinite fold repeatedly producing its input.
--
-- @
-- 'Fresnel.Fold.toListOf' 'repeated' a = 'repeat' a
-- @
repeated :: Fold1 a a
repeated = rphantom . traversal1 (\ f a -> let loop = f a .> loop in loop)


-- Elimination

foldMap1Of :: Semigroup m => Fold1 s a -> ((a -> m) -> (s -> m))
foldMap1Of o = runForget #. o .# Forget

foldMap1ByOf :: Fold1 s a -> ((r -> r -> r) -> (a -> r) -> (s -> r))
foldMap1ByOf o fork leaf s = runFork1 (runForget (o (Forget Fork1.singleton)) s) fork leaf

foldrMap1Of :: Fold1 s a -> ((a -> r) -> (a -> r -> r) -> (s -> r))
foldrMap1Of o last cons s = runCons1 (runForget (o (Forget Cons1.singleton)) s) last cons

foldlMap1Of :: Fold1 s a -> ((a -> r) -> (r -> a -> r) -> (s -> r))
foldlMap1Of o first snoc s = runSnoc1 (runForget (o (Forget Snoc1.singleton)) s) first snoc

fold1Of :: Semigroup a => Fold1 s a -> (s -> a)
fold1Of o = foldMap1Of o id

fold1ByOf :: Fold1 s a -> ((a -> a -> a) -> (s -> a))
fold1ByOf o fork s = runFork1 (runForget (o (Forget Fork1.singleton)) s) fork id

sequence1Of_ :: Apply f => Fold1 s (f a) -> (s -> f ())
sequence1Of_ o = runTraversed1 . foldMap1Of o Traversed1

traverse1Of_ :: Apply f => Fold1 s a -> ((a -> f r) -> (s -> f ()))
traverse1Of_ o f = runTraversed1 . foldMap1Of o (Traversed1 #. f)

for1Of_ :: Apply f => Fold1 s a -> (s -> (a -> f r) -> f ())
for1Of_ o = flip (traverse1Of_ o)

toList1Of :: Fold1 s a -> (s -> NonEmpty a)
toList1Of o = foldMap1Of o pure

concatOf :: Fold1 s (NonEmpty a) -> (s -> NonEmpty a)
concatOf = fold1Of

concatMapOf :: Fold1 s a -> ((a -> NonEmpty r) -> (s -> NonEmpty r))
concatMapOf = foldMap1Of

firstOf :: Fold1 s a -> (s -> a)
firstOf o = getFirst #. foldMap1Of o First

lastOf :: Fold1 s a -> (s -> a)
lastOf o = getLast #. foldMap1Of o Last

minimumOf :: Ord a => Fold1 s a -> (s -> a)
minimumOf o = minimumByOf o compare

minimumByOf :: Fold1 s a -> (a -> a -> Ordering) -> (s -> a)
minimumByOf o cmp = foldlMap1Of o id (\ a b -> case cmp a b of
  GT -> b
  _  -> a)

maximumOf :: Ord a => Fold1 s a -> (s -> a)
maximumOf o = maximumByOf o compare

maximumByOf :: Fold1 s a -> (a -> a -> Ordering) -> (s -> a)
maximumByOf o cmp = foldlMap1Of o id (\ a b -> case cmp a b of
  LT -> b
  _  -> a)


-- Union semigroup

newtype Union s a = Union { getUnion1 :: Fold1 s a }

instance Semigroup (Union s a) where
    Union a1 <> Union a2 = Union (rphantom . traversal1 (\ f s -> traverse1Of_ a1 f s .> traverse1Of_ a2 f s) . rphantom)
