{-# LANGUAGE TemplateHaskell #-}
module Getter.Test
( test
) where

import Test.QuickCheck

pure []

test :: IO Bool
test = $quickCheckAll
