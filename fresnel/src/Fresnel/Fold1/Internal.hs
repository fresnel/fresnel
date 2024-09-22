module Fresnel.Fold1.Internal
( IsFold1
) where

import Data.Profunctor (Forget)
import Fresnel.Getter.Internal (IsGetter)
import Fresnel.Traversal1.Internal (IsTraversal1)

class (IsGetter p, IsTraversal1 p) => IsFold1 p

instance Semigroup r => IsFold1 (Forget r)
