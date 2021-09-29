module Fresnel.Optional.Internal
( IsOptional
) where

import Control.Arrow (Kleisli)
import Data.Profunctor (Forget, Star)
import Fresnel.Lens.Internal (IsLens)
import Fresnel.Prism.Internal (IsPrism)
import Fresnel.Profunctor.OptionalStar (OptionalStar)

class (IsLens p, IsPrism p) => IsOptional p where

instance IsOptional (->)
instance Monad m => IsOptional (Kleisli m)
instance Monoid r => IsOptional (Forget r)
instance Applicative f => IsOptional (Star f)
instance Functor f => IsOptional (OptionalStar f)
