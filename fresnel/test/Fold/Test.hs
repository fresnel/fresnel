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
prop_union_semigroup_assoc (ArbFold a) (ArbFold b) (ArbFold c) as = foldMapOf (getUnion (a <> (b <> c))) (:[]) as === foldMapOf (getUnion ((a <> b) <> c)) (:[]) as

prop_union_monoid_left_identity :: (Eq a, Show a) => ArbFold a -> [a] -> Property
prop_union_monoid_left_identity (ArbFold a) as = foldMapOf (getUnion (mempty <> a)) (:[]) as === foldMapOf (getUnion a) (:[]) as

prop_union_monoid_right_identity :: (Eq a, Show a) => ArbFold a -> [a] -> Property
prop_union_monoid_right_identity (ArbFold a) as = foldMapOf (getUnion (a <> mempty)) (:[]) as === foldMapOf (getUnion a) (:[]) as


newtype ArbFold a = ArbFold (Union [a] a)

instance Show a => Show (ArbFold a) where
  showsPrec _ (ArbFold (Union fold)) = showList (foldMapOf fold (:[]) []) -- FIXME: this is a bad instance

instance Arbitrary a => Arbitrary (ArbFold a) where
  arbitrary = oneof
    [ pure (ArbFold (Union folded))
    , pure (ArbFold (Union ignored))
    , ArbFold . ixed <$> arbitrary
    ]
    where
    ixed :: Int -> Union [a] a
    ixed i = Union (ix i)


pure []

test :: IO Bool
test = $quickCheckAll
