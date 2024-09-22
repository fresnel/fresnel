module Fresnel.Traversal1.Internal
( IsTraversal1
) where

import Data.Functor.Apply
import Data.Profunctor (Forget, Star)
import Fresnel.Lens.Internal (IsLens)
import Fresnel.Profunctor.Traversing1 (Traversing1)

class (IsLens p, Traversing1 p) => IsTraversal1 p

instance Semigroup r => IsTraversal1 (Forget r)
instance Apply f => IsTraversal1 (Star f)
