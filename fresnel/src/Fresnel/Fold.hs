{-# LANGUAGE RankNTypes #-}
module Fresnel.Fold
( -- * Folds
  Fold
, IsFold
  -- * Construction
, folded
  -- * Elimination
, foldMapOf
, foldOf
, (^?)
) where

import Data.Foldable (traverse_)
import Data.Monoid
import Data.Profunctor
import Data.Profunctor.Traversing
import Data.Profunctor.Unsafe ((#.), (.#))
import Fresnel.Bifunctor.Contravariant
import Fresnel.Optic
import Fresnel.Profunctor.Optical

-- Folds

type Fold s a = forall p . IsFold p => Optic' p s a

class (IsOptionalFold p, IsTraversal p) => IsFold p

instance Monoid r => IsFold (Forget r)


-- Construction

folded :: Foldable f => Fold (f a) a
folded = rphantom . wander traverse_


-- Elimination

foldMapOf :: Monoid m => Fold s a -> ((a -> m) -> (s -> m))
foldMapOf o = runForget #. o .# Forget

foldOf :: Monoid a => Fold s a -> (s -> a)
foldOf o = runForget (o (Forget id))


(^?) :: s -> Fold s a -> Maybe a
s ^? l = getFirst (foldMapOf l (First #. Just) s)

infixl 8 ^?
