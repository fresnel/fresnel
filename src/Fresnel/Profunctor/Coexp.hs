module Fresnel.Profunctor.Coexp
( -- * Coexponential profunctor
  Coexp(..)
  -- * Elimination
, withCoexp
) where

import Data.Profunctor

-- Coexponential

-- | Coexponentials are the dual of functions, consisting of an argument of type @a@ (derived within an environment of type @e@) and a continuation from the return type @b@ (extending to the eventual result type @r@). As such, they naturally have the shape of optics, relating the outer context @e -> r@ to the inner @a -> b@.
--
-- The record selector names were chosen to indicate that 'Coexp' is essentially the pairing of 'Forget' and 'Fresnel.Profunctor.Recall'.
data Coexp e r a b = Coexp { recall :: e -> b, forget :: a -> r }
  deriving (Functor)

instance Monoid r => Applicative (Coexp e r b) where
  pure a = Coexp (pure a) mempty
  Coexp f kf <*> Coexp a ka = Coexp (f <*> a) (mappend <$> kf <*> ka)

instance Profunctor (Coexp e r) where
  dimap f g c = Coexp (g . recall c) (forget c . f)


-- Elimination

withCoexp :: Coexp e r a b -> ((e -> b) -> (a -> r) -> x) -> x
withCoexp (Coexp r f) k = k r f
