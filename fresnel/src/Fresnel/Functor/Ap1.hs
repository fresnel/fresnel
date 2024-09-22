module Fresnel.Functor.Ap1
( Ap1(..)
) where

newtype Ap1 f a = Ap1 { getAp1 :: f a }
