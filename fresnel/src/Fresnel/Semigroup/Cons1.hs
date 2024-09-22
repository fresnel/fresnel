{-# LANGUAGE RankNTypes #-}
module Fresnel.Semigroup.Cons1
( -- * Non-empty cons lists
  Cons1(..)
) where

import Data.Foldable (toList)
import Data.Foldable1

-- Non-empty cons lists

newtype Cons1 a = Cons1 { runCons1 :: forall r . (a -> r) -> (a -> r -> r) -> r }

instance Show a => Show (Cons1 a) where
  showsPrec _ = showList . toList

instance Foldable Cons1 where
  foldMap f (Cons1 r) = r f ((<>) . f)
  foldr f z (Cons1 r) = r (`f` z) f

instance Foldable1 Cons1 where
  foldMap1 f (Cons1 r) = r f ((<>) . f)
  foldrMap1 f g (Cons1 r) = r f g
