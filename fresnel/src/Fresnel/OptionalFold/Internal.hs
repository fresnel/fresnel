module Fresnel.OptionalFold.Internal
( IsOptionalFold
) where

import Data.Functor.Contravariant
import Data.Profunctor
import Fresnel.Getter (IsGetter)
import Fresnel.Optional.Internal (IsOptional)
import Fresnel.Profunctor.OptionalStar (OptionalStar)

class (IsOptional p, IsGetter p) => IsOptionalFold p

instance Monoid r => IsOptionalFold (Forget r)
instance (Applicative f, Traversable f, Contravariant f) => IsOptionalFold (Star f)
instance (Traversable f, Contravariant f) => IsOptionalFold (OptionalStar f)
