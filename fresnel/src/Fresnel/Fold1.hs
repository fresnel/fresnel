{-# LANGUAGE RankNTypes #-}
module Fresnel.Fold1
( Fold1
) where

import Fresnel.Fold1.Internal (IsFold1)
import Fresnel.Optic (Optic')

type Fold1 s a = forall p . IsFold1 p => Optic' p s a
