module Fresnel.Functor.Ap1
( Ap1(..)
) where

newtype Ap1 f a = Ap1 { getAp1 :: f a }

instance Functor f => Functor (Ap1 f) where
  fmap f (Ap1 g) = Ap1 (fmap f g)
