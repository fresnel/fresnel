{-# LANGUAGE RankNTypes #-}
module Fresnel.Traversal1
( Traversal1
, IsTraversal1
) where

import Fresnel.Optic
import Fresnel.Traversal1.Internal

type Traversal1 s t a b = forall p . IsTraversal1 p => Optic p s t a b
