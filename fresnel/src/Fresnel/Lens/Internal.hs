{-# LANGUAGE RankNTypes #-}
module Fresnel.Lens.Internal
( IsLens
) where

import Control.Arrow (Kleisli)
import Data.Profunctor (Forget, Star, Strong)
import Fresnel.Iso.Internal (IsIso)
import Fresnel.Profunctor.OptionalStar (OptionalStar)

class (IsIso p, Strong p) => IsLens p

instance IsLens (->)
instance Monad m => IsLens (Kleisli m)
instance IsLens (Forget r)
instance Functor f => IsLens (Star f)
instance Functor f => IsLens (OptionalStar f)
