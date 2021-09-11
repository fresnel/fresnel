{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Fold.Test
( test
) where

import Fresnel.Fold
import Fresnel.Ixed
import Test.QuickCheck

prop_union_semigroup_assoc :: (Eq a, Show a) => ArbFold a -> ArbFold a -> ArbFold a -> [a] -> Property
prop_union_semigroup_assoc (ArbFold a) (ArbFold b) (ArbFold c) as = foldMapOf (getUnion (Union a <> (Union b <> Union c))) (:[]) as === foldMapOf (getUnion ((Union a <> Union b) <> Union c)) (:[]) as

prop_union_monoid_left_identity :: (Eq a, Show a) => ArbFold a -> [a] -> Property
prop_union_monoid_left_identity (ArbFold a) as = foldMapOf (getUnion (mempty <> Union a)) (:[]) as === foldMapOf a (:[]) as

prop_union_monoid_right_identity :: (Eq a, Show a) => ArbFold a -> [a] -> Property
prop_union_monoid_right_identity (ArbFold a) as = foldMapOf (getUnion (Union a <> mempty)) (:[]) as === foldMapOf a (:[]) as


prop_failover_semigroup_assoc :: (Eq a, Show a) => ArbFold a -> ArbFold a -> ArbFold a -> [a] -> Property
prop_failover_semigroup_assoc (ArbFold a) (ArbFold b) (ArbFold c) as = foldMapOf (getFailover (Failover a <> (Failover b <> Failover c))) (:[]) as === foldMapOf (getFailover ((Failover a <> Failover b) <> Failover c)) (:[]) as

prop_failover_monoid_left_identity :: (Eq a, Show a) => ArbFold a -> [a] -> Property
prop_failover_monoid_left_identity (ArbFold a) as = foldMapOf (getFailover (mempty <> Failover a)) (:[]) as === foldMapOf a (:[]) as

prop_failover_monoid_right_identity :: (Eq a, Show a) => ArbFold a -> [a] -> Property
prop_failover_monoid_right_identity (ArbFold a) as = foldMapOf (getFailover (Failover a <> mempty)) (:[]) as === foldMapOf a (:[]) as


newtype ArbFold a = ArbFold (Fold [a] a)

instance Show a => Show (ArbFold a) where
  showsPrec _ (ArbFold fold) = showList (foldMapOf fold (:[]) []) -- FIXME: this is a bad instance

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

test :: IO Bool
test = $quickCheckAll
