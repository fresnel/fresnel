module Fresnel.Functor.Traversed1
( -- * Traversed1 functor
  runTraversed1
, Traversed1(..)
) where

import Data.Functor (void)
import Data.Functor.Apply

runTraversed1 :: Functor f => Traversed1 f a -> f ()
runTraversed1 (Traversed1 fa) = void fa

newtype Traversed1 f a = Traversed1 (f a)

instance Apply f => Semigroup (Traversed1 f a) where
  Traversed1 a1 <> Traversed1 a2 = Traversed1 (a1 .> a2)
