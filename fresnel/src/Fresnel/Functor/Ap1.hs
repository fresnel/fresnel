module Fresnel.Functor.Ap1
( Ap1(..)
) where

import Data.Functor.Apply

newtype Ap1 f a = Ap1 { getAp1 :: f a }

instance Functor f => Functor (Ap1 f) where
  fmap f (Ap1 g) = Ap1 (fmap f g)

instance Apply f => Apply (Ap1 f) where
  liftF2 f (Ap1 a) (Ap1 b) = Ap1 (liftF2 f a b)
