module Fresnel.Profunctor.Market
( -- * Market profunctor
  Market(..)
) where

import Data.Bifunctor (first)
import Data.Profunctor

-- Market profunctor

data Market a b s t = Market { inj :: b -> t, prj :: s -> Either t a }

instance Functor (Market a b s) where
  fmap = rmap

instance Profunctor (Market a b) where
  dimap f g (Market inj prj) = Market (g . inj) (first g . prj . f)
