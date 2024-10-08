{-# LANGUAGE TupleSections #-}
module Fresnel.Profunctor.Star1
( -- * Star1 profunctor
  Star1(..)
) where

import Data.Functor.Apply
import Data.Functor.Contravariant (Contravariant(..))
import Data.Profunctor
import Fresnel.Bifunctor.Contravariant (Bicontravariant(..))
import Fresnel.Profunctor.Traversing1

-- | Just like 'Data.Profunctor.Star', but with some instances defined in terms of 'Data.Functor.Apply' instead of 'Applicative'. Used by 'Fresnel.Traversal1.Traversal1' & 'Fresnel.Fold1.Fold1'.
newtype Star1 f a b = Star1 { runStar1 :: a -> f b }

instance Functor f => Profunctor (Star1 f) where
  dimap f g (Star1 h) = Star1 (fmap g . h . f)

instance Functor f => Strong (Star1 f) where
  first'  (Star1 h) = Star1 (\ (a, c) -> (,c) <$> h a)
  second' (Star1 h) = Star1 (\ (c, a) -> (c,) <$> h a)

instance Traversable f => Cochoice (Star1 f) where
  unright (Star1 h) = Star1 (go . Right)
    where
    go = either (go . Left) id . sequence . h

instance Apply f => Traversing1 (Star1 f) where
  wander1 f (Star1 h) = Star1 (f h)

instance Contravariant f => Bicontravariant (Star1 f) where
  contrabimap f g (Star1 h) = Star1 (contramap g . h . f)
