module Fresnel.Traversal1.Internal
( IsTraversal1
) where

import Data.Profunctor (Forget)
import Fresnel.Lens.Internal (IsLens)
import Fresnel.Profunctor.Traversing1 (Traversing1)

class (IsLens p, Traversing1 p) => IsTraversal1 p

instance Semigroup r => IsTraversal1 (Forget r)
