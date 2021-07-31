{-# LANGUAGE TemplateHaskell #-}
module Iso.Test
( test
) where

import Test.QuickCheck


pure []

test :: IO Bool
test = $quickCheckAll
