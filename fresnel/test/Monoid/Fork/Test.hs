{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Monoid.Fork.Test
( tests
) where

import Test.QuickCheck

pure []

tests :: (String, [(String, Property)])
tests = (__FILE__, $allProperties)
