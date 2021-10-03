{-# LANGUAGE TemplateHaskell #-}
module Tropical.Test
( tests
) where

import Data.Monoid (Sum)
import Fresnel.Tropical
import Test.Group
import Test.QuickCheck hiding ((><))

prop_semigroup_assoc (ArbTropical a) (ArbTropical b) (ArbTropical c) = label (summarize a) . label (summarize b) . label (summarize c) $ a <> (b <> c) === (a <> b) <> c

prop_monoid_identity (ArbTropical a) = label (summarize a) $ (mempty <> a) === a .&&. (a <> mempty) === a

prop_semiring_assoc (ArbTropical a) (ArbTropical b) (ArbTropical c) = label (summarize a) . label (summarize b) . label (summarize c) $ a >< (b >< c) === (a >< b) >< c

prop_unital_identity (ArbTropical a) = label (summarize a) $ (one >< a) === a .&&. (a >< one) === a


summarize :: Tropical (Sum Int) -> String
summarize (Tropical a) = case a of
  Nothing          -> "zero"
  Just a
    | signum a > 0 -> "pos"
    | signum a < 0 -> "neg"
    | otherwise    -> "one"


newtype ArbTropical = ArbTropical (Tropical (Sum Int))
  deriving (Eq, Ord, Show)

instance Arbitrary ArbTropical where
  arbitrary = oneof $ map (fmap (ArbTropical . Tropical))
    [ pure Nothing
    , Just <$> arbitrary
    , pure (Just 0)
    ]


pure []

tests :: Entry
tests = $deriveGroup
