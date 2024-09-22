{-# LANGUAGE RankNTypes #-}
module Fresnel.Traversal1
( -- * Relevant traversals
  Traversal1
, Traversal1'
, IsTraversal1
  -- * Construction
, traversal1
) where

import Data.Functor.Apply
import Fresnel.Optic
import Fresnel.Profunctor.Traversing1
import Fresnel.Traversal1.Internal

-- Relevant traversals

type Traversal1 s t a b = forall p . IsTraversal1 p => Optic p s t a b

type Traversal1' s a = Traversal1 s s a a


-- Construction

traversal1 :: (forall f . Apply f => (a -> f b) -> (s -> f t)) -> Traversal1 s t a b
traversal1 = wander1
