{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fresnel.Functor.Backwards
( Backwards(..)
) where

import Data.Functor.Apply (Apply)

newtype Backwards f a = Backwards { forwards :: f a }
  deriving (Applicative, Apply, Functor)
