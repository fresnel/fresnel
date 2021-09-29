{-# LANGUAGE TemplateHaskell #-}
module Review.Test
( tests
) where
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Test.Group
import Test.QuickCheck


pure []

tests :: Group
tests = mkGroup ($(thisModule >>= \ (Module _ name) -> stringE (modString name)), $allProperties)
