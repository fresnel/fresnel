module Fresnel.Iso.Internal
( IsIso
) where

import Control.Arrow (Kleisli)
import Data.Profunctor
import Fresnel.Profunctor.Coexp (Coexp)
import Fresnel.Profunctor.OptionalStar (OptionalStar)
import Fresnel.Profunctor.Recall (Recall)
import Fresnel.Profunctor.Star1 (Star1)

class Profunctor p => IsIso p

instance IsIso (->)
instance Monad m => IsIso (Kleisli m)
instance IsIso (Forget r)
instance IsIso (Recall e)
instance Functor f => IsIso (Star f)
instance Functor f => IsIso (Costar f)
instance Functor f => IsIso (OptionalStar f)
instance Functor f => IsIso (Star1 f)
instance IsIso (Coexp s t)
