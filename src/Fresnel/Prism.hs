{-# LANGUAGE RankNTypes #-}
module Fresnel.Prism
( -- * Prisms
  Prism
, Prism'
  -- * Construction
, prism
, prism'
  -- * Elimination
, withPrism
, matching
, isn't
  -- * Relations
, only
  -- * Either
, _Left
, _Right
  -- * Maybe
, _Just
, _Nothing
) where

import Control.Monad (guard)
import Data.Profunctor
import Fresnel.Optic
import Fresnel.Profunctor.Market

-- Prisms

type Prism s t a b = forall p . Choice p => Optic p s t a b

type Prism' s a = Prism s s a a


-- Construction

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism inj prj = dimap prj (either id inj) . right'

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' inj prj = prism inj (\ s -> maybe (Left s) Right (prj s))


-- Elimination

withPrism :: Prism s t a b -> (((b -> t) -> (s -> Either t a) -> r) -> r)
withPrism o = withMarket (o (Market id Right))

matching :: Prism s t a b -> (s -> Either t a)
matching o = withPrism o (const id)

isn't :: Prism s t a b -> (s -> Bool)
isn't o = either (const True) (const False) . matching o


-- Relations

only :: Eq a => a -> Prism' a ()
only a = prism' (const a) (guard . (== a))


-- Either

_Left :: Prism (Either a b) (Either a' b) a a'
_Left = prism Left (either Right (Left . Right))

_Right :: Prism (Either a b) (Either a b') b b'
_Right = prism Right (either (Left . Left) Right)


-- Maybe

_Just :: Prism (Maybe a) (Maybe a') a a'
_Just = prism Just (maybe (Left Nothing) Right)

_Nothing :: Prism' (Maybe a) ()
_Nothing = prism' (const Nothing) (maybe (Just ()) (const Nothing))
