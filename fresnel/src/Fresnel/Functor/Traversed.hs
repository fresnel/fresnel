{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fresnel.Functor.Traversed
( -- * Traversed functor
  runTraversed
, Traversed(..)
) where

import Data.Functor (void)

-- Traversed functor

runTraversed :: Functor f => Traversed f a -> f ()
runTraversed (Traversed fa) = void fa

newtype Traversed f a = Traversed (f a)
  deriving (Applicative, Functor)

instance Applicative f => Semigroup (Traversed f a) where
  Traversed a1 <> Traversed a2 = Traversed (a1 *> a2)

instance Applicative f => Monoid (Traversed f a) where
  mempty = Traversed (pure (error "Traversed.mempty: value used"))
