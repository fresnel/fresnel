{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Getter.Test
( tests
) where

import Fresnel.Getter
import Test.QuickCheck

prop_view_to_involution f x = view (to (applyFun f)) x === applyFun f x


pure []

tests :: (String, [(String, Property)])
tests = (__FILE__, $allProperties)
