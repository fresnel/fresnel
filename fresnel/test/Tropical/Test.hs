{-# LANGUAGE TemplateHaskell #-}
module Tropical.Test
( tests
) where

import Fresnel.Tropical
import Test.Group
import Test.QuickCheck

prop_semigroup_assoc (ArbTropical a) (ArbTropical b) (ArbTropical c) = label (summarize a) . label (summarize b) . label (summarize c) $ a <> (b <> c) === (a <> b) <> c

prop_monoid_identity (ArbTropical a) = label (summarize a) $ (mempty <> a) === a .&&. (a <> mempty) === a


summarize :: Tropical Int -> String
summarize (Tropical a) = case a of
  Nothing          -> "zero"
  Just a
    | signum a > 0 -> "pos"
    | signum a < 0 -> "neg"
    | otherwise    -> "one"


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
