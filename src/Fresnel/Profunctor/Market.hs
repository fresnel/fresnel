module Fresnel.Profunctor.Market
( Market(..)
) where

import Data.Bifunctor (first)
import Data.Profunctor

data Market a b s t = Market { inj :: b -> t, prj :: s -> Either t a }

instance Profunctor (Market a b) where
  dimap f g (Market inj prj) = Market (g . inj) (first g . prj . f)
