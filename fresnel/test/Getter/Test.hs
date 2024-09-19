{-# LANGUAGE TemplateHaskell #-}
module Getter.Test
( tests
) where

import Fresnel.Getter
import Test.Group
import Test.QuickCheck

prop_view_to_involution (Fn f) x = view (to f) x === f x


pure []

tests :: Entry
tests = $deriveGroup
