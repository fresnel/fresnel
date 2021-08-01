{-# LANGUAGE FlexibleInstances #-}
module Fresnel.Profunctor.Coexp
( -- * Coexponential profunctor
  Coexp(..)
  -- * Construction
, coexp
  -- * Elimination
, withCoexp
, recall
, forget
) where

import Data.Profunctor

-- Coexponential

-- | Coexponentials are the dual of functions, consisting of an argument of type @a@ (derived within an environment of type @s@) and a continuation from the return type @b@ (extending to the eventual result type @t@). As such, they naturally have the shape of optics, relating the outer context @s -> t@ to the inner @a -> b@.
data Coexp s t b a = Coexp (s -> a) (b -> t)

instance Functor (Coexp s t b) where
  fmap = rmap

instance Monoid t => Applicative (Coexp s t b) where
  pure a = coexp (pure a) mempty
  f <*> a = withCoexp f $ \ f kf -> withCoexp a $ \ a ka -> coexp (f <*> a) (mappend <$> kf <*> ka)

instance Profunctor (Coexp s t) where
  dimap f g c = withCoexp c $ \ recall forget -> coexp (g . recall) (forget . f)

instance Semigroup (Coexp a b b a) where
  c1 <> c2 = withCoexp c1 $ \ r1 f1 -> withCoexp c2 $ \ r2 f2 -> coexp (r2 . r1) (f1 . f2)

instance Monoid (Coexp a b b a) where
  mempty = coexp id id


-- Construction

coexp :: (s -> a) -> (b -> t) -> Coexp s t b a
coexp = Coexp


-- Elimination

withCoexp :: Coexp s t b a -> ((s -> a) -> (b -> t) -> x) -> x
withCoexp (Coexp r f) k = k r f

recall :: Coexp s t b a -> (s -> a)
recall c = withCoexp c const

forget :: Coexp s t b a -> (b -> t)
forget c = withCoexp c (const id)
