{-# LANGUAGE TemplateHaskell #-}
module Fold.Test
( test
) where

import Test.QuickCheck

pure []

test :: IO Bool
test = $quickCheckAll
