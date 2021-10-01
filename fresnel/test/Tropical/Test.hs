{-# LANGUAGE TemplateHaskell #-}
module Tropical.Test
( tests
) where

import Fresnel.Tropical
import Test.Group
import Test.QuickCheck

prop_semigroup_assoc (ArbTropical a) (ArbTropical b) (ArbTropical c) = a <> (b <> c) === (a <> b) <> c

prop_monoid_identity (ArbTropical a) = (mempty <> a) === a .&&. (a <> mempty) === a


newtype ArbTropical = ArbTropical (Tropical Int)
  deriving (Eq, Ord, Show)

instance Arbitrary ArbTropical where
  arbitrary = oneof $ map (fmap (ArbTropical . Tropical))
    [ pure Nothing
    , Just <$> arbitrary
    ]


pure []

tests :: Entry
tests = $deriveGroup
