module Fresnel.Traversal1.Internal
( IsTraversal1
) where

import Control.Arrow (Kleisli)
import Data.Functor.Apply
import Data.Profunctor (Forget, Star)
import Fresnel.Lens.Internal (IsLens)
import Fresnel.Profunctor.OptionalStar (OptionalStar)
import Fresnel.Profunctor.Star1 (Star1)
import Fresnel.Profunctor.Traversing1 (Traversing1)

class (IsLens p, Traversing1 p) => IsTraversal1 p

instance IsTraversal1 (->)
instance Monad m => IsTraversal1 (Kleisli m)
instance Semigroup r => IsTraversal1 (Forget r)
instance Apply f => IsTraversal1 (Star f)
instance Apply f => IsTraversal1 (Star1 f)
instance Apply f => IsTraversal1 (OptionalStar f)
