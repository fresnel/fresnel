{-# LANGUAGE TemplateHaskell #-}
module Getter.Test
( tests
) where

import Fresnel.Getter
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Test.Group
import Test.QuickCheck

prop_view_to_involution f x = view (to (applyFun f)) x === applyFun f x


pure []

tests :: Group
tests = mkGroup ($(thisModule >>= \ (Module _ name) -> stringE (modString name)), $allProperties)
