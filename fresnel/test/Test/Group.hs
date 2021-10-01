{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Group
( Entry(..)
, mkGroup
, deriveGroup
, mkProp
, entryName
, Loc(..)
, here
, zero
, Semiring(..)
, Unital(..)
, horizontal
, sumWidths
, vertical
, maxWidths
, Width(..)
, HasWidth(..)
) where

import Data.Char (isSpace)
import Fresnel.Tropical
import GHC.Stack
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax (Module(..), modString)
import Numeric (readDec)
import Test.QuickCheck (Property, allProperties)

data Entry
  = Group String [Entry]
  | Prop String Loc Property

mkGroup :: String -> [(String, Property)] -> Entry
mkGroup name = Group name . map (uncurry mkProp)

deriveGroup :: ExpQ
deriveGroup = [e| mkGroup $(thisModule >>= \ (Module _ name) -> stringE (modString name)) $allProperties |]

mkProp :: String -> Property -> Entry
mkProp s = Prop name Loc{ path, lineNumber }
  where
  (name, path, lineNumber) = case breaks [isSpace, not . isSpace, isSpace, not . isSpace, (== ':'), (/= ':')] s of
    [n, _, _, _, p, _, l] -> (unwords (filter (\ s -> s /= "_" && s /= "prop") (breakAll (== '_') n)), p, fst (head (readDec l)))
    _                     -> ("", "", 0)

entryName :: Entry -> String
entryName = \case
  Group name _  -> name
  Prop name _ _ -> name


data Loc = Loc { path :: FilePath, lineNumber :: Int }

here :: HasCallStack => Loc
here = Loc{ path = srcLocFile srcLoc, lineNumber = srcLocStartLine srcLoc }
  where
  (_, srcLoc) = head (getCallStack callStack)


breaks :: [a -> Bool] -> [a] -> [[a]]
breaks ps as = case ps of
  []   -> [as]
  p:ps -> let (h, t) = break p as in h : breaks ps t

breakAll :: (a -> Bool) -> [a] -> [[a]]
breakAll p = go False where
  go b = \case
    [] -> []
    as -> let (h, t) = break (if b then not . p else p) as in h : go (not b) t


zero :: Monoid s => s
zero = mempty

class Semigroup s => Semiring s where
  (><) :: s -> s -> s
  infixr 7 ><

instance (Semigroup a, Ord a) => Semiring (Tropical a) where
  Tropical a1 >< Tropical a2 = Tropical ((<>) <$> a1 <*> a2)

class (Monoid s, Semiring s) => Unital s where
  one :: s

instance (Monoid a, Ord a) => Unital (Tropical a) where
  one = finite zero


horizontal :: (Foldable t, Unital r) => (a -> r) -> t a -> r
horizontal f = foldr ((><) . f) one

sumWidths :: (Foldable t, HasWidth a) => t a -> Tropical Width
sumWidths = horizontal maxWidth

vertical :: (Foldable t, Semiring r, Monoid r) => (a -> r) -> t a -> r
vertical f = foldr ((<>) . f) zero

maxWidths :: (Foldable t, HasWidth a) => t a -> Tropical Width
maxWidths = vertical maxWidth

newtype Width = Width { width :: Int }
  deriving (Eq, Ord)

instance Semigroup Width where
  Width a1 <> Width a2 = Width (a1 + a2)

instance Monoid Width where
  mempty = Width 0

instance Semiring Width where
  Width a1 >< Width a2 = Width (a1 * a2)

instance Unital Width where
  one = Width 1


class HasWidth t where
  maxWidth :: t -> Tropical Width

instance HasWidth Char where
  maxWidth _ = finite one

instance HasWidth Entry where
  maxWidth = \case
    Group groupName entries -> sumWidths groupName <> maxWidths entries
    Prop name _ _           -> sumWidths name
