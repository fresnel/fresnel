module Fresnel.Fold1.Internal
( IsFold1
) where

import Fresnel.Getter.Internal (IsGetter)
import Fresnel.Traversal1.Internal (IsTraversal1)

class (IsGetter p, IsTraversal1 p) => IsFold1 p