{-# LANGUAGE RankNTypes #-}
module Fresnel.Traversal1
( -- * Relevant traversals
  Traversal1
, Traversal1'
, IsTraversal1
  -- * Construction
, traversal1
, traversed1
) where

import Data.Functor.Apply
import Data.Semigroup.Traversable
import Fresnel.Optic
import Fresnel.Profunctor.Traversing1
import Fresnel.Traversal1.Internal

-- Relevant traversals

type Traversal1 s t a b = forall p . IsTraversal1 p => Optic p s t a b

type Traversal1' s a = Traversal1 s s a a


-- Construction

traversal1 :: (forall f . Apply f => (a -> f b) -> (s -> f t)) -> Traversal1 s t a b
traversal1 = wander1

traversed1 :: Traversable1 t => Traversal1 (t a) (t b) a b
traversed1 = traversal1 traverse1
