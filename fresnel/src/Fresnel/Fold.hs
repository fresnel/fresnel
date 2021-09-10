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
  -- * Elimination
, foldMapOf
, foldOf
, (^?)
, Union(..)
) where

import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Monoid
import Data.Profunctor
import Data.Profunctor.Traversing
import Data.Profunctor.Unsafe ((#.), (.#))
import Fresnel.Bifunctor.Contravariant
import Fresnel.Optic
import Fresnel.OptionalFold (IsOptionalFold)
import Fresnel.Traversal (IsTraversal)

-- Folds

type Fold s a = forall p . IsFold p => Optic' p s a

class (IsOptionalFold p, IsTraversal p) => IsFold p

instance Monoid r => IsFold (Forget r)


-- Construction

folded :: Foldable f => Fold (f a) a
folded = rphantom . wander traverse_

unfolded :: (s -> Maybe (a, s)) -> Fold s a
unfolded coalg = rphantom . wander (\ f -> let loop = maybe (pure ()) (\ (a, s) -> f a *> loop s) . coalg in loop)

folding :: Foldable f => (s -> f a) -> Fold s a
folding f = contrabimap f (const ()) . rmap (const ()) . wander traverse_

foldring :: (forall f . Applicative f => (a -> f u -> f u) -> f v -> s -> f w) -> Fold s a
foldring fr = rphantom . wander (\ f -> runTraversed . fr (\ a -> (Traversed (f a) *>)) mempty)


-- Elimination

foldMapOf :: Monoid m => Fold s a -> ((a -> m) -> (s -> m))
foldMapOf o = runForget #. o .# Forget

foldOf :: Monoid a => Fold s a -> (s -> a)
foldOf o = runForget (o (Forget id))

traverseOf_ :: Applicative f => Fold s a -> (a -> f r) -> (s -> f ())
traverseOf_ o f = runTraversed . foldMapOf o (Traversed #. f)


(^?) :: s -> Fold s a -> Maybe a
s ^? l = getFirst (foldMapOf l (First #. Just) s)

infixl 8 ^?


newtype Union s a = Union { getUnion :: Fold s a }

instance Semigroup (Union s a) where
  Union a1 <> Union a2 = Union (rphantom . wander (\ f s -> traverseOf_ a1 f s *> traverseOf_ a2 f s) . rphantom)


newtype Traversed f a = Traversed (f a)
  deriving (Applicative, Functor)

runTraversed :: Functor f => Traversed f a -> f ()
runTraversed (Traversed fa) = void fa

instance Applicative f => Semigroup (Traversed f a) where
  Traversed a1 <> Traversed a2 = Traversed (a1 *> a2)

instance Applicative f => Monoid (Traversed f a) where
  mempty = Traversed (pure (error "Traversed.mempty: value used"))
