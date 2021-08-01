{-# LANGUAGE FlexibleInstances #-}
module Fresnel.Profunctor.Coexp
( -- * Coexponential profunctor
  Coexp(..)
  -- * Elimination
, withCoexp
) where

import Data.Profunctor

-- Coexponential

-- | Coexponentials are the dual of functions, consisting of an argument of type @a@ (derived within an environment of type @s@) and a continuation from the return type @b@ (extending to the eventual result type @t@). As such, they naturally have the shape of optics, relating the outer context @s -> t@ to the inner @a -> b@.
--
-- The record selector names were chosen to indicate that 'Coexp' is essentially the pairing of 'Forget' and 'Fresnel.Profunctor.Recall'.
data Coexp s t b a = Coexp { recall :: s -> a, forget :: b -> t }

instance Functor (Coexp s t b) where
  fmap = rmap

instance Monoid t => Applicative (Coexp s t b) where
  pure a = Coexp (pure a) mempty
  Coexp f kf <*> Coexp a ka = Coexp (f <*> a) (mappend <$> kf <*> ka)

instance Profunctor (Coexp s t) where
  dimap f g c = Coexp (g . recall c) (forget c . f)

instance Semigroup (Coexp a b b a) where
  Coexp r1 f1 <> Coexp r2 f2 = Coexp (r2 . r1) (f1 . f2)

instance Monoid (Coexp a b b a) where
  mempty = Coexp id id


-- Elimination

withCoexp :: Coexp s t b a -> ((s -> a) -> (b -> t) -> x) -> x
withCoexp (Coexp r f) k = k r f
