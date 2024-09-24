{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fresnel.Functor.Ap1
( Ap1(..)
) where

import Data.Functor.Apply

newtype Ap1 f a = Ap1 { getAp1 :: f a }
  deriving (Applicative, Apply, Functor, Monad)

instance (Apply f, Semigroup a) => Semigroup (Ap1 f a) where
  Ap1 a <> Ap1 b = Ap1 (liftF2 (<>) a b)
