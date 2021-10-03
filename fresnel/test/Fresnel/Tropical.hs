module Fresnel.Tropical
( finite
, Tropical(..)
) where

finite :: a -> Tropical a
finite = Tropical . Just

newtype Tropical a = Tropical { getTropical :: Maybe a }
  deriving (Eq, Ord, Show)

instance Ord a => Semigroup (Tropical a) where
  (<>) = max

instance Ord a => Monoid (Tropical a) where
  mempty = Tropical Nothing
