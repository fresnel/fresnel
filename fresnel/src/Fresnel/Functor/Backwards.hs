{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fresnel.Functor.Backwards
( Backwards(..)
) where

newtype Backwards f a = Backwards { forwards :: f a }
  deriving (Applicative, Functor)
