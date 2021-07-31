module Fresnel.Traversal
( -- * Traversals
  Traversal
, Traversal'
  -- * Construction
, traversed
) where

import Data.Profunctor.Traversing
import Fresnel.Optic

-- Traversals

type Traversal s t a b = forall p . Traversing p => Optic p s t a b

type Traversal' s a = Traversal s s a a


-- Construction

traversed :: Traversable t => Traversal (t a) (t b) a b
traversed = wander traverse
