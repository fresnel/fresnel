{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fresnel.Functor.Ap1
( Ap1(..)
) where

import Data.Functor.Apply

newtype Ap1 f a = Ap1 { getAp1 :: f a }
  deriving (Applicative, Apply, Functor, Monad)
