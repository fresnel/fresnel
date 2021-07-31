module Fresnel.Profunctor.Market
( -- * Market profunctor
  Market(..)
  -- * Elimination
, withMarket
) where

import Data.Bifunctor (first)
import Data.Profunctor

-- Market profunctor

data Market a b s t = Market { inj :: b -> t, prj :: s -> Either t a }

instance Functor (Market a b s) where
  fmap = rmap

instance Profunctor (Market a b) where
  dimap f g (Market inj prj) = Market (g . inj) (first g . prj . f)

instance Choice (Market a b) where
  left' (Market inj prj) = Market (Left . inj) (either (either (Left . Left) Right . prj) (Left . Right))


-- Elimination

withMarket :: Market a b s t -> (((b -> t) -> (s -> Either t a) -> r) -> r)
withMarket (Market inj prj) f = f inj prj
