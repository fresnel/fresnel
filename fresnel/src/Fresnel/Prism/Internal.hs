module Fresnel.Prism.Internal
( IsPrism
) where

import Control.Arrow (Kleisli)
import Data.Profunctor (Choice, Forget, Star)
import Fresnel.Iso.Internal
import Fresnel.Profunctor.OptionalStar (OptionalStar)
import Fresnel.Profunctor.Recall (Recall)

class (IsIso p, Choice p) => IsPrism p

instance IsPrism (->)
instance Monad m => IsPrism (Kleisli m)
instance Monoid r => IsPrism (Forget r)
instance IsPrism (Recall e)
instance Applicative f => IsPrism (Star f)
instance Functor f => IsPrism (OptionalStar f)
