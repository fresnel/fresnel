{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Monoid.Fork.Test
( tests
) where

import Data.Foldable (toList)
import Fresnel.Monoid.Fork
import Test.QuickCheck

prop_semigroup_assoc :: (Eq a, Show a) => ArbFork a -> ArbFork a -> ArbFork a -> Property
prop_semigroup_assoc (ArbFork a) (ArbFork b) (ArbFork c) =
  toList (a <> (b <> c)) === toList ((a <> b) <> c)

prop_monoid_left_identity :: (Eq a, Show a) => ArbFork a -> Property
prop_monoid_left_identity (ArbFork a) = toList (mempty <> a) === toList a


newtype ArbFork a = ArbFork (Fork a)
  deriving (Show)

instance Arbitrary a => Arbitrary (ArbFork a) where
  arbitrary = ArbFork <$> sized go where
    go 0 = pure mempty
    go i = oneof
      [ chooseInt (0, i) >>= \ j -> (<>) <$> go (i - j) <*> go j
      , singleton <$> arbitrary
      , pure mempty
      ]



pure []

tests :: (String, [(String, Property)])
tests = (__FILE__, $allProperties)
