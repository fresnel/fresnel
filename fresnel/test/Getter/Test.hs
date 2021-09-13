{-# LANGUAGE TemplateHaskell #-}
module Getter.Test
( tests
) where

import Fresnel.Getter
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Test.QuickCheck

prop_view_to_involution f x = view (to (applyFun f)) x === applyFun f x


pure []

tests :: (String, [(String, Property)])
tests = ($(thisModule >>= \ (Module _ name) -> stringE (modString name)), $allProperties)
