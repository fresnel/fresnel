{-# LANGUAGE TemplateHaskell #-}
module Iso.Test
( test
) where

import Fresnel.Getter
import Fresnel.Iso
import Test.QuickCheck

prop_view_elimination f g x = view (iso (applyFun f) (applyFun g)) x === applyFun f x


pure []

test :: IO Bool
test = $quickCheckAll
