{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Monoid.Fork.Test
( tests
) where

import Data.Foldable (toList)
import Fresnel.Monoid.Fork (Fork(runFork), singleton)
import Test.QuickCheck

prop_semigroup_assoc :: (Eq a, Show a) => ArbFork a -> ArbFork a -> ArbFork a -> Property
prop_semigroup_assoc (ArbFork a) (ArbFork b) (ArbFork c) =
  label (summarize a) . label (summarize b) . label (summarize c) $
  (toList (a <> (b <> c)) === toList ((a <> b) <> c))

prop_monoid_identity :: (Eq a, Show a) => ArbFork a -> Property
prop_monoid_identity (ArbFork a) = label (summarize a) $ toList (mempty <> a) === toList a .&&. toList (a <> mempty) === toList a


newtype ArbFork a = ArbFork (Fork a)
  deriving (Show)

data Kind = Nil | Leaf | Fork
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ArbFork a) where
  arbitrary = ArbFork <$> sized go where
    go 0 = pure mempty
    go i = oneof
      [ chooseInt (0, i) >>= \ j -> (<>) <$> go (i - j) <*> go j
      , singleton <$> arbitrary
      , pure mempty
      ]


summarize :: Fork a -> String
summarize r = runFork r (\ _ _ -> "fork") (const "leaf") "nil"


pure []

tests :: (String, [(String, Property)])
tests = (__FILE__, $allProperties)
