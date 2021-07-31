module Fresnel.Traversal
( -- * Traversals
  Traversal
, Traversal'
  -- * Construction
, traversed
  -- * Elimination
, traverseOf
) where

import Data.Profunctor
import Data.Profunctor.Traversing
import Fresnel.Optic

-- Traversals

type Traversal s t a b = forall p . Traversing p => Optic p s t a b

type Traversal' s a = Traversal s s a a


-- Construction

traversed :: Traversable t => Traversal (t a) (t b) a b
traversed = wander traverse


-- Elimination

traverseOf :: Applicative f => Traversal s t a b -> ((a -> f b) -> (s -> f t))
traverseOf o = runStar . o . Star
