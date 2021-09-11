{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Fresnel.Fold
( -- * Folds
  Fold
, IsFold
  -- * Construction
, folded
, unfolded
, folding
, foldring
, ignored
  -- * Elimination
, foldMapOf
, foldMapByOf
, foldrOf
, foldOf
, traverseOf_
, toListOf
, previews
, preview
, (^?)
, Failover(..)
, Union(..)
) where

import Data.Foldable (traverse_)
import Data.Functor.Contravariant
import Data.Monoid
import Data.Profunctor
import Data.Profunctor.Traversing
import Data.Profunctor.Unsafe ((#.), (.#))
import Fresnel.Bifunctor.Contravariant
import Fresnel.Functor.Traversed
import Fresnel.Monoid.Cons as Cons
import Fresnel.Monoid.Fork as Fork
import Fresnel.Optic
import Fresnel.OptionalFold.Internal (IsOptionalFold)
import Fresnel.Traversal (IsTraversal)

-- Folds

type Fold s a = forall p . IsFold p => Optic' p s a

class (IsOptionalFold p, IsTraversal p) => IsFold p

instance Monoid r => IsFold (Forget r)
instance (Applicative f, Traversable f, Contravariant f) => IsFold (Star f)


-- Construction

folded :: Foldable f => Fold (f a) a
folded = rphantom . wander traverse_

unfolded :: (s -> Maybe (a, s)) -> Fold s a
unfolded coalg = rphantom . wander (\ f -> let loop = maybe (pure ()) (\ (a, s) -> f a *> loop s) . coalg in loop)

folding :: Foldable f => (s -> f a) -> Fold s a
folding f = contrabimap f (const ()) . rmap (const ()) . wander traverse_

foldring :: (forall f . Applicative f => (a -> f u -> f u) -> f v -> s -> f w) -> Fold s a
foldring fr = rphantom . wander (\ f -> fr (\ a -> (f a *>)) (pure v)) where
  v = error "foldring: value used"

ignored :: Fold s a
ignored = foldring (\ _ nil _ -> nil)


-- Elimination

foldMapOf :: Monoid m => Fold s a -> ((a -> m) -> (s -> m))
foldMapOf o = runForget #. o .# Forget

foldMapByOf :: Fold s a -> ((r -> r -> r) -> r -> (a -> r) -> (s -> r))
foldMapByOf o fork nil leaf s = runFork (runForget (o (Forget Fork.singleton)) s) fork leaf nil

foldrOf :: Fold s a -> ((a -> r -> r) -> r -> s -> r)
foldrOf o cons nil s = runCons (runForget (o (Forget Cons.singleton)) s) cons nil

foldOf :: Monoid a => Fold s a -> (s -> a)
foldOf o = foldMapOf o id

traverseOf_ :: Applicative f => Fold s a -> ((a -> f r) -> (s -> f ()))
traverseOf_ o f = runTraversed . foldMapOf o (Traversed #. f)

toListOf :: Fold s a -> s -> [a]
toListOf o = foldrOf o (:) []


previews :: Fold s a -> (a -> r) -> (s -> Maybe r)
previews o f = getFirst #. foldMapOf o (First #. Just . f)

preview :: Fold s a -> s -> Maybe a
preview o = previews o id

(^?) :: s -> Fold s a -> Maybe a
s ^? o = preview o s

infixl 8 ^?


newtype Failover s a = Failover { getFailover :: Fold s a }

instance Semigroup (Failover s a) where
  Failover a1 <> Failover a2 = Failover (folding (\ s -> Cons (\ cons nil -> maybe (foldrOf a2 cons nil s) (uncurry cons) (foldrOf a1 (\ a -> Just . (,) a . maybe nil (uncurry cons)) Nothing s))))

instance Monoid (Failover s a) where
  mempty = Failover ignored


newtype Union s a = Union { getUnion :: Fold s a }

instance Semigroup (Union s a) where
  Union a1 <> Union a2 = Union (rphantom . wander (\ f s -> traverseOf_ a1 f s *> traverseOf_ a2 f s) . rphantom)

instance Monoid (Union s a) where
  mempty = Union ignored
