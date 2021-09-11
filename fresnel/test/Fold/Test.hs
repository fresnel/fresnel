{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Fold.Test
( test
) where

import Fresnel.Fold
import Test.QuickCheck

prop_union_semigroup_assoc :: (Eq a, Show a) => [a] -> Property
prop_union_semigroup_assoc as = foldMapOf (getUnion (union <> (union <> union))) (:[]) as === foldMapOf (getUnion ((union <> union) <> union)) (:[]) as


union :: Union [a] a
union = Union folded


pure []

test :: IO Bool
test = $quickCheckAll
