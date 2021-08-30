{-# LANGUAGE RankNTypes #-}
module Fresnel.Traversal
( -- * Traversals
  Traversal
, Traversal'
, Traversing(..)
  -- * Construction
, traversed
, backwards
  -- * Elimination
, traverseOf
) where

import Control.Applicative.Backwards
import Data.Profunctor
import Fresnel.Profunctor.Optical
import Fresnel.Optic

-- Traversals

type Traversal s t a b = forall p . Traversing p => Optic p s t a b

type Traversal' s a = Traversal s s a a


-- Construction

traversed :: Traversable t => Traversal (t a) (t b) a b
traversed = wander traverse

backwards :: Traversal s t a b -> Traversal s t a b
backwards o = wander (\ f -> forwards . traverseOf o (Backwards . f))


-- Elimination

traverseOf :: Applicative f => Traversal s t a b -> ((a -> f b) -> (s -> f t))
traverseOf o = runStar . o . Star
