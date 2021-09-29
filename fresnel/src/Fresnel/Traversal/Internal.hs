module Fresnel.Traversal.Internal
( IsTraversal
) where

import Control.Arrow (Kleisli)
import Data.Profunctor (Forget, Star)
import Data.Profunctor.Traversing (Traversing)
import Fresnel.Optional.Internal (IsOptional)

class (IsOptional p, Traversing p) => IsTraversal p

instance IsTraversal (->)
instance Monad m => IsTraversal (Kleisli m)
instance Monoid r => IsTraversal (Forget r)
instance Applicative f => IsTraversal (Star f)
