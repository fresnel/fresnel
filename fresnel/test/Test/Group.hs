{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
) where

import Data.Char (isSpace)
import GHC.Stack
import Numeric (readDec)
import Test.QuickCheck (Property)

data Group = Group
  { groupName :: String
  , cases     :: [Case]
  }

mkGroup :: (String, [(String, Property)]) -> Group
mkGroup = uncurry Group . fmap (map (uncurry mkCase))

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

instance (Num a, Ord a) => Semiring (Tropical a) where
  Tropical a1 >< Tropical a2 = Tropical ((+) <$> a1 <*> a2)

instance (Num a, Ord a) => Unital (Tropical a) where
  one = finite 0

finite :: a -> Tropical a
finite = Tropical . Just

horizontal :: (Foldable t, Unital r) => (a -> r) -> t a -> r
horizontal f = foldr ((><) . f) one

sumWidths :: (Foldable t, Width a) => t a -> Tropical Int
sumWidths = horizontal width

vertical :: (Foldable t, Semiring r, Monoid r) => (a -> r) -> t a -> r
vertical f = foldr ((<>) . f) zero
maxWidths :: (Foldable t, Width a) => t a -> Tropical Int
maxWidths = vertical width


class Width t where
  width :: t -> Tropical Int

instance Width Char where
  width _ = finite 1

instance Width Group where
  width Group{ groupName, cases } = sumWidths groupName <> maxWidths cases

instance Width Case where
  width Case{ name } = sumWidths name
