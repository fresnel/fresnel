{-# LANGUAGE TemplateHaskell #-}
module Monoid.Fork.Test
( test
) where

import Test.QuickCheck

pure []

test :: IO Bool
test = $quickCheckAll
