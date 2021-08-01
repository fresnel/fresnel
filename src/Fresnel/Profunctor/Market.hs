{-# LANGUAGE RankNTypes #-}
module Fresnel.Profunctor.Market
( -- * Market profunctor
  Market(..)
) where

import Data.Bifunctor (first)
import Data.Profunctor

-- Market profunctor

newtype Market a b s t = Market { withMarket :: forall r . ((b -> t) -> (s -> Either t a) -> r) -> r }

instance Functor (Market a b s) where
  fmap = rmap

instance Profunctor (Market a b) where
  dimap f g (Market r) = r $ \ inj prj -> Market $ \ k -> k (g . inj) (first g . prj . f)

instance Choice (Market a b) where
  left' (Market r) = r $ \ inj prj -> Market $ \ k -> k (Left . inj) (either (either (Left . Left) Right . prj) (Left . Right))
