{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Fold.Test
( tests
) where

import Fresnel.Fold
import Fresnel.Ixed
import Test.Group
import Test.QuickCheck

prop_union_semigroup_assoc :: (Eq a, Show a) => ArbFold a -> ArbFold a -> ArbFold a -> [a] -> Property
prop_union_semigroup_assoc (ArbFold a) (ArbFold b) (ArbFold c) as = classifyList as $ toListOf (getUnion (Union a <> (Union b <> Union c))) as === toListOf (getUnion ((Union a <> Union b) <> Union c)) as

prop_union_monoid_identity :: (Eq a, Show a) => ArbFold a -> [a] -> Property
prop_union_monoid_identity (ArbFold a) as =
  classifyList as $
  toListOf (getUnion (mempty <> Union a)) as === toListOf a as .&&. toListOf (getUnion (Union a <> mempty)) as === toListOf a as


prop_failover_semigroup_assoc :: (Eq a, Show a) => ArbFold a -> ArbFold a -> ArbFold a -> [a] -> Property
prop_failover_semigroup_assoc (ArbFold a) (ArbFold b) (ArbFold c) as = classifyList as $ toListOf (getFailover (Failover a <> (Failover b <> Failover c))) as === toListOf (getFailover ((Failover a <> Failover b) <> Failover c)) as

prop_failover_monoid_identity :: (Eq a, Show a) => ArbFold a -> [a] -> Property
prop_failover_monoid_identity (ArbFold a) as = classifyList as $ toListOf (getFailover (mempty <> Failover a)) as === toListOf a as .&&. toListOf (getFailover (Failover a <> mempty)) as === toListOf a as


prop_iterated (Fn f) a (MostlyPositive n) = classifyInt n $ take n (toListOf (iterated f) a) === take n (iterate f a)


prop_filtered (Fn p) as
  = classify (not (any p as)) "reject all"
  . classify (all p as) "accept all"
  . classifyList as
  $ toListOf (folded.filtered p) as === filter p as


prop_repeated (MostlyPositive n) a = classifyInt n $ take n (toListOf repeated a) === take n (repeat a)


prop_replicated (MostlyPositive n) a = classifyInt n $ toListOf (replicated n) a === replicate n a


prop_cycled (NonEmpty as) (NonNegative n) = classifyList as $ take n (toListOf (cycled folded) as) === take n (cycle as)


prop_takingWhile (Fn p) as = classifyList as $ toListOf (takingWhile p folded) as === takeWhile p as


classifyList :: Testable prop => [a] -> prop -> Property
classifyList as = classify (null as) "empty" . classify (length as == 1) "singleton"

classifyInt :: Testable prop => Int -> prop -> Property
classifyInt n = classify (n == 0) "zero" . classify (n < 0) "negative"


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


newtype MostlyPositive a = MostlyPositive { getMostlyPositive :: a }
  deriving (Eq, Num, Ord, Show)

instance (Arbitrary a, Num a, Ord a) => Arbitrary (MostlyPositive a) where
  arbitrary = MostlyPositive <$> frequency
    [ (90, getPositive <$> arbitrary)
    , (5, pure 0)
    , (5, getNegative <$> arbitrary)
    ]

  shrink (MostlyPositive a) = [MostlyPositive a' | a' <- shrink a, signum a' == signum a]


pure []

tests :: Entry
tests = $deriveGroup
