{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Fold.Test
( test
) where

import Fresnel.Fold
import Fresnel.Ixed
import Test.QuickCheck

prop_union_semigroup_assoc :: (Eq a, Show a) => ArbUnion a -> ArbUnion a -> ArbUnion a -> [a] -> Property
prop_union_semigroup_assoc (ArbUnion a) (ArbUnion b) (ArbUnion c) as = foldMapOf (getUnion (a <> (b <> c))) (:[]) as === foldMapOf (getUnion ((a <> b) <> c)) (:[]) as

prop_union_monoid_left_identity :: (Eq a, Show a) => ArbUnion a -> [a] -> Property
prop_union_monoid_left_identity (ArbUnion a) as = foldMapOf (getUnion (mempty <> a)) (:[]) as === foldMapOf (getUnion a) (:[]) as

prop_union_monoid_right_identity :: (Eq a, Show a) => ArbUnion a -> [a] -> Property
prop_union_monoid_right_identity (ArbUnion a) as = foldMapOf (getUnion (a <> mempty)) (:[]) as === foldMapOf (getUnion a) (:[]) as


newtype ArbUnion a = ArbUnion (Union [a] a)

instance Show a => Show (ArbUnion a) where
  showsPrec _ (ArbUnion (Union fold)) = showList (foldMapOf fold (:[]) []) -- FIXME: this is a bad instance

instance Arbitrary a => Arbitrary (ArbUnion a) where
  arbitrary = oneof
    [ pure (ArbUnion (Union folded))
    , pure (ArbUnion (Union ignored))
    , ArbUnion . ixed <$> arbitrary
    ]
    where
    ixed :: Int -> Union [a] a
    ixed i = Union (ix i)


pure []

test :: IO Bool
test = $quickCheckAll
