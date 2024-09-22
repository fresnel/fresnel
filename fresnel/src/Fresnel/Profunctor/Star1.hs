module Fresnel.Profunctor.Star1
( -- * Star1 profunctor
  Star1(..)
) where

-- | Just like 'Data.Profunctor.Star', but with some instances defined in terms of 'Data.Functor.Apply' instead of 'Applicative'. Used by 'Fresnel.Traversal1.Traversal1' & 'Fresnel.Fold1.Fold1'.
newtype Star1 f a b = Star1 { runStar1 :: a -> f b }
