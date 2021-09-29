{-# LANGUAGE RankNTypes #-}
module Fresnel.Traversal
( -- * Traversals
  Traversal
, Traversal'
, IsTraversal
  -- * Construction
, traversed
, backwards
  -- * Elimination
, traverseOf
) where

import Data.Profunctor
import Data.Profunctor.Traversing (Traversing(..))
import Fresnel.Functor.Backwards
import Fresnel.Optic
import Fresnel.Traversal.Internal (IsTraversal)

-- Traversals

type Traversal s t a b = forall p . IsTraversal p => Optic p s t a b

type Traversal' s a = Traversal s s a a


-- Construction

traversed :: Traversable t => Traversal (t a) (t b) a b
traversed = wander traverse

backwards :: Traversal s t a b -> Traversal s t a b
backwards o = wander (\ f -> forwards . traverseOf o (Backwards . f))


-- Elimination

traverseOf :: Applicative f => Traversal s t a b -> ((a -> f b) -> (s -> f t))
traverseOf o = runStar . o . Star
