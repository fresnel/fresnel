{-# LANGUAGE TemplateHaskell #-}
module Getter.Test
( test
) where

import Fresnel.Getter
import Test.QuickCheck

prop_view_to_involution f x = view (to (applyFun f)) x === applyFun f x


pure []

test :: IO Bool
test = $quickCheckAll
