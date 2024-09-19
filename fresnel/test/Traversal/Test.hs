{-# LANGUAGE TemplateHaskell #-}
module Traversal.Test
( tests
) where

import Test.Group

pure []

tests :: Entry
tests = $deriveGroup
