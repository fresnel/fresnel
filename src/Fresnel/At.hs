{-# LANGUAGE TypeFamilies #-}
module Fresnel.At
( -- * Indexable collections
  Ixed(..)
) where

-- Indexable collections

class Ixed c where
  type Index c
  type IxValue c
