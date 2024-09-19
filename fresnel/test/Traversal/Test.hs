{-# LANGUAGE TemplateHaskell #-}
module Traversal.Test
( tests
) where

import Fresnel.Traversal
import Test.Group
import Test.QuickCheck

prop_ignored (Fn f) a = traverseOf ignored f a === [a]


pure []

tests :: Entry
tests = $deriveGroup
