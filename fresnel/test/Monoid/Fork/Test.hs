{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Monoid.Fork.Test
( tests
) where

import Fresnel.Monoid.Fork
import Test.QuickCheck

prop_semigroup_assoc :: (Eq a, Show a) => ArbFork a -> ArbFork a -> ArbFork a -> Property
prop_semigroup_assoc (ArbFork a) (ArbFork b) (ArbFork c) =
  runFork (a <> (b <> c)) (++) (:[]) [] === runFork ((a <> b) <> c) (++) (:[]) []


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
