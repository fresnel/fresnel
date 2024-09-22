{-# LANGUAGE TupleSections #-}
module Fresnel.Profunctor.Star1
( -- * Star1 profunctor
  Star1(..)
) where

import Data.Profunctor

-- | Just like 'Data.Profunctor.Star', but with some instances defined in terms of 'Data.Functor.Apply' instead of 'Applicative'. Used by 'Fresnel.Traversal1.Traversal1' & 'Fresnel.Fold1.Fold1'.
newtype Star1 f a b = Star1 { runStar1 :: a -> f b }

instance Functor f => Profunctor (Star1 f) where
  dimap f g (Star1 h) = Star1 (fmap g . h . f)

instance Functor f => Strong (Star1 f) where
  first'  (Star1 h) = Star1 (\ (a, c) -> (,c) <$> h a)
  second' (Star1 h) = Star1 (\ (c, a) -> (c,) <$> h a)
