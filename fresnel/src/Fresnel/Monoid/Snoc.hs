{-# LANGUAGE RankNTypes #-}
module Fresnel.Monoid.Snoc
( -- * Snoc lists
  Snoc(..)
  -- * Construction
, singleton
, snoc
, nil
) where

import Data.Foldable (toList)

-- Snoc lists

newtype Snoc a = Snoc { runSnoc :: forall r . (r -> a -> r) -> r -> r }

instance Show a => Show (Snoc a) where
  showsPrec _ = showList . toList

instance Semigroup (Snoc a) where
  Snoc a1 <> Snoc a2 = Snoc (\ snoc -> a1 snoc . a2 snoc)

instance Monoid (Snoc a) where
  mempty = nil

instance Foldable Snoc where
  foldMap f (Snoc r) = r (const <> const f) mempty

instance Functor Snoc where
  fmap f (Snoc r) = r ((. f) . snoc) nil


-- Construction

singleton :: a -> Snoc a
singleton a = Snoc (\ snoc nil -> snoc nil a)

snoc :: Snoc a -> a -> Snoc a
snoc (Snoc as) a = Snoc (\ snoc nil -> snoc (as snoc nil) a)

nil :: Snoc a
nil = Snoc (\ _ nil -> nil)
