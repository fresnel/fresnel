{-# LANGUAGE RankNTypes #-}
module Fresnel.OptionalFold
( -- * Optional folds
  OptionalFold
, IsOptionalFold
  -- * Construction
, folding
, filtered
  -- * Elimination
, previews
, preview
, (^?)
, isn't
, traverseOf_
, Failover(..)
) where

import Control.Applicative ((<|>))
import Data.Functor (void)
import Data.Functor.Contravariant (Contravariant)
import Data.Maybe (isJust)
import Data.Monoid (First(..))
import Data.Profunctor
import Data.Profunctor.Unsafe
import Fresnel.Bifunctor.Contravariant
import Fresnel.Getter (IsGetter)
import Fresnel.Optic
import Fresnel.Optional (IsOptional)
import Fresnel.Profunctor.OptionalStar (OptionalStar)

-- Optional folds

type OptionalFold s a = forall p . IsOptionalFold p => Optic' p s a

class (IsOptional p, IsGetter p) => IsOptionalFold p

instance Monoid r => IsOptionalFold (Forget r)
instance (Applicative f, Traversable f, Contravariant f) => IsOptionalFold (Star f)
instance (Traversable f, Contravariant f) => IsOptionalFold (OptionalStar f)


-- Construction

folding :: (s -> Maybe a) -> OptionalFold s a
folding f = contrabimap ((`maybe` Right) . Left <*> f) Left . right'

filtered :: (a -> Bool) -> OptionalFold a a
filtered p = folding (\ a -> if p a then Just a else Nothing)


-- Elimination

previews :: OptionalFold s a -> (a -> r) -> (s -> Maybe r)
previews o f = getFirst #. runForget (o (Forget (First #. Just . f)))

preview :: OptionalFold s a -> s -> Maybe a
preview o = previews o id

(^?) :: s -> OptionalFold s a -> Maybe a
s ^? o = preview o s

infixl 8 ^?

isn't :: OptionalFold s a -> s -> Bool
isn't o = isJust . preview o

traverseOf_ :: Functor f => OptionalFold s a -> ((forall x . x -> f x) -> (a -> f u) -> (s -> f ()))
traverseOf_ o point f s = maybe (point ()) (void . f) (preview o s)


newtype Failover s a = Failover { getFailover :: OptionalFold s a }

instance Semigroup (Failover s a) where
  Failover a1 <> Failover a2 = Failover (folding (\ s -> preview a1 s <|> preview a2 s))
