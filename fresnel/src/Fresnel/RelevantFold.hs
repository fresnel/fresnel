{-# LANGUAGE RankNTypes #-}
module Fresnel.RelevantFold
( RelevantFold
) where

import Fresnel.Optic
import Fresnel.Traversal.Internal (IsTraversal)

type RelevantFold s a = forall p . IsRelevantFold p => Optic' p s a

class IsTraversal p => IsRelevantFold p
