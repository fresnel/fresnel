{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Group
( Group(..)
, mkGroup
, Case(..)
, mkCase
, Loc(..)
, here
, zero
, Semiring(..)
, Unital(..)
, Tropical(..)
, finite
, horizontal
, sumWidths
, vertical
, maxWidths
, Width(..)
, HasWidth(..)
) where

import Data.Char (isSpace)
import GHC.Stack
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax (Module(..), modString)
import Numeric (readDec)
import Test.QuickCheck (Property, allProperties)

data Group = Group
  { groupName :: String
  , cases     :: [Case]
  }

mkGroup :: ExpQ
mkGroup = [e| Group $(thisModule >>= \ (Module _ name) -> stringE (modString name)) (map (uncurry mkCase) $allProperties) |]

data Case = Case
  { name     :: String
  , loc      :: Loc
  , property :: Property
  }

mkCase :: String -> Property -> Case
mkCase s property = Case{ name, loc = Loc{ path, lineNumber }, property }
  where
  (name, path, lineNumber) = case breaks [isSpace, not . isSpace, isSpace, not . isSpace, (== ':'), (/= ':')] s of
    [n, _, _, _, p, _, l] -> (unwords (filter (\ s -> s /= "_" && s /= "prop") (breakAll (== '_') n)), p, fst (head (readDec l)))
    _                     -> ("", "", 0)


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

class (Monoid s, Semiring s) => Unital s where
  one :: s


newtype Tropical a = Tropical { getTropical :: Maybe a }
  deriving (Eq, Ord, Show)

instance Ord a => Semigroup (Tropical a) where
  (<>) = max

instance Ord a => Monoid (Tropical a) where
  mempty = Tropical Nothing

instance (Semigroup a, Ord a) => Semiring (Tropical a) where
  Tropical a1 >< Tropical a2 = Tropical ((<>) <$> a1 <*> a2)

instance (Monoid a, Ord a) => Unital (Tropical a) where
  one = finite zero

finite :: a -> Tropical a
finite = Tropical . Just

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

instance HasWidth Group where
  maxWidth Group{ groupName, cases } = sumWidths groupName <> maxWidths cases

instance HasWidth Case where
  maxWidth Case{ name } = sumWidths name
