{-# LANGUAGE RankNTypes #-}
module Fresnel.Semigroup.Cons1
( -- * Non-empty cons lists
  Cons1(..)
) where

import Data.Foldable (toList)

-- Non-empty cons lists

newtype Cons1 a = Cons1 { runCons1 :: forall r . (a -> r) -> (a -> r -> r) -> r }

instance Show a => Show (Cons1 a) where
  showsPrec _ = showList . toList

instance Foldable Cons1 where
  foldMap f (Cons1 r) = r f ((<>) . f)
  foldr f z (Cons1 r) = r (`f` z) f
