module Fresnel.Traversal
( -- * Traversals
  Traversal
, Traversal'
) where

import Data.Profunctor.Traversing
import Fresnel.Optic

-- Traversals

type Traversal s t a b = forall p . Traversing p => Optic p s t a b

type Traversal' s a = Traversal s s a a
