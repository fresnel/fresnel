module Fresnel.Getter.Internal
( IsGetter
) where

import Data.Functor.Contravariant (Contravariant)
import Data.Profunctor (Cochoice, Forget, Star)
import Fresnel.Bifunctor.Contravariant (Bicontravariant)
import Fresnel.Lens.Internal (IsLens)
import Fresnel.Profunctor.OptionalStar (OptionalStar)
import Fresnel.Profunctor.Star1 (Star1)

class (IsLens p, Bicontravariant p, Cochoice p) => IsGetter p

instance IsGetter (Forget r)
instance (Contravariant f, Traversable f) => IsGetter (Star f)
instance (Contravariant f, Traversable f) => IsGetter (Star1 f)
instance (Contravariant f, Traversable f) => IsGetter (OptionalStar f)
