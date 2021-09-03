{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Fresnel.At
( -- * Indexable collections
  Ixed(..)
) where

import Fresnel.Traversal

-- Indexable collections

class Ixed c where
  type Index c
  type IxValue c

  ix :: Index c -> Traversal' c (IxValue c)
