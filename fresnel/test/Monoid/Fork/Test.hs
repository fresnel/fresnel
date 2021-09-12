{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Monoid.Fork.Test
( tests
) where

import Data.Foldable (toList)
import Fresnel.Monoid.Fork (Fork, singleton)
import Test.QuickCheck

prop_semigroup_assoc :: (Eq a, Show a) => ArbFork a -> ArbFork a -> ArbFork a -> Property
prop_semigroup_assoc (ArbFork ka a) (ArbFork kb b) (ArbFork kc c) =
  label (show ka) . label (show kb) . label (show kc) $
  (toList (a <> (b <> c)) === toList ((a <> b) <> c))

prop_monoid_identity :: (Eq a, Show a) => ArbFork a -> Property
prop_monoid_identity (ArbFork ka a) = label (show ka) $ toList (mempty <> a) === toList a .&&. toList (a <> mempty) === toList a


data ArbFork a = ArbFork Kind (Fork a)
  deriving (Show)

data Kind = Nil | Leaf | Fork
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ArbFork a) where
  arbitrary = uncurry ArbFork <$> sized go where
    go 0 = pure (Nil, mempty)
    go i = oneof
      [ do
        j <- chooseInt (0, i)
        (_, l) <- go (i - j)
        (_, r) <- go j
        pure (Fork, l <> r)
      , (,) Leaf . singleton <$> arbitrary
      , pure (Nil, mempty)
      ]


pure []

tests :: (String, [(String, Property)])
tests = (__FILE__, $allProperties)
