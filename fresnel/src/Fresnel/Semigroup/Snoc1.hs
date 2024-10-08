{-# LANGUAGE RankNTypes #-}
module Fresnel.Semigroup.Snoc1
( -- * Non-empty snoc lists
  Snoc1(..)
  -- * Construction
, singleton
, snoc
) where

import Data.Foldable (toList)
import Data.Foldable1

-- Non-empty snoc lists

newtype Snoc1 a = Snoc1 { runSnoc1 :: forall r . (a -> r) -> (r -> a -> r) -> r }

instance Show a => Show (Snoc1 a) where
  showsPrec _ = showList . toList

instance Semigroup (Snoc1 a) where
  Snoc1 a1 <> Snoc1 a2 = Snoc1 (\ f g -> a2 (\ a -> g (a1 f g) a) g)

instance Foldable Snoc1 where
  foldMap f (Snoc1 r) = r f ((. f) . (<>))
  foldl f z (Snoc1 r) = r (z `f`) f

instance Foldable1 Snoc1 where
  foldMap1 f (Snoc1 r) = r f ((. f) . (<>))
  foldlMap1 f g (Snoc1 r) = r f g

instance Functor Snoc1 where
  fmap h (Snoc1 r) = Snoc1 (\ f g -> r (f . h) ((. h) . g))


-- Construction

singleton :: a -> Snoc1 a
singleton a = Snoc1 (\ f _ -> f a)

snoc :: Snoc1 a -> a -> Snoc1 a
snoc (Snoc1 r) a = Snoc1 (\ f g -> g (r f g) a)
