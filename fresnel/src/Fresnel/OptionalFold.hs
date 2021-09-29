{-# LANGUAGE RankNTypes #-}
module Fresnel.OptionalFold
( -- * Optional folds
  OptionalFold
, IsOptionalFold
  -- * Construction
, folding
, filtered
  -- * Elimination
, is
, isn't
, traverseOf_
, Failover(..)
) where

import Control.Applicative ((<|>))
import Data.Functor (void)
import Data.Maybe (isJust, isNothing)
import Data.Profunctor
import Fresnel.Bifunctor.Contravariant
import Fresnel.Fold (preview)
import Fresnel.Optic
import Fresnel.OptionalFold.Internal

-- Optional folds

type OptionalFold s a = forall p . IsOptionalFold p => Optic' p s a


-- Construction

folding :: (s -> Maybe a) -> OptionalFold s a
folding f = contrabimap ((`maybe` Right) . Left <*> f) Left . right'

filtered :: (a -> Bool) -> OptionalFold a a
filtered p = folding (\ a -> if p a then Just a else Nothing)


-- Elimination

is :: OptionalFold s a -> (s -> Bool)
is o = isNothing . preview o

isn't :: OptionalFold s a -> (s -> Bool)
isn't o = isJust . preview o

traverseOf_ :: Functor f => OptionalFold s a -> ((forall x . x -> f x) -> (a -> f u) -> (s -> f ()))
traverseOf_ o point f s = maybe (point ()) (void . f) (preview o s)


newtype Failover s a = Failover { getFailover :: OptionalFold s a }

instance Semigroup (Failover s a) where
  Failover a1 <> Failover a2 = Failover (folding (\ s -> preview a1 s <|> preview a2 s))
