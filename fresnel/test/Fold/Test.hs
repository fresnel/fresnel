{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Fold.Test
( tests
) where

import Fresnel.Fold
import Fresnel.Ixed
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Test.QuickCheck

prop_union_semigroup_assoc :: (Eq a, Show a) => ArbFold a -> ArbFold a -> ArbFold a -> [a] -> Property
prop_union_semigroup_assoc (ArbFold a) (ArbFold b) (ArbFold c) as = toListOf (getUnion (Union a <> (Union b <> Union c))) as === toListOf (getUnion ((Union a <> Union b) <> Union c)) as

prop_union_monoid_identity :: (Eq a, Show a) => ArbFold a -> [a] -> Property
prop_union_monoid_identity (ArbFold a) as = toListOf (getUnion (mempty <> Union a)) as === toListOf a as .&&. toListOf (getUnion (Union a <> mempty)) as === toListOf a as


prop_failover_semigroup_assoc :: (Eq a, Show a) => ArbFold a -> ArbFold a -> ArbFold a -> [a] -> Property
prop_failover_semigroup_assoc (ArbFold a) (ArbFold b) (ArbFold c) as = toListOf (getFailover (Failover a <> (Failover b <> Failover c))) as === toListOf (getFailover ((Failover a <> Failover b) <> Failover c)) as

prop_failover_monoid_identity :: (Eq a, Show a) => ArbFold a -> [a] -> Property
prop_failover_monoid_identity (ArbFold a) as = toListOf (getFailover (mempty <> Failover a)) as === toListOf a as .&&. toListOf (getFailover (Failover a <> mempty)) as === toListOf a as


newtype ArbFold a = ArbFold (Fold [a] a)

instance Show a => Show (ArbFold a) where
  showsPrec _ (ArbFold fold) = showList (toListOf fold []) -- FIXME: this is a bad instance

instance Arbitrary a => Arbitrary (ArbFold a) where
  arbitrary = oneof
    [ pure (ArbFold folded)
    , pure (ArbFold ignored)
    , ixed <$> arbitrary
    ]
    where
    ixed :: Int -> ArbFold a
    ixed i = ArbFold (ix i)


pure []

tests :: (String, [(String, Property)])
tests = ($(thisModule >>= \ (Module _ name) -> stringE (modString name)), $allProperties)
