{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Test.Group
( Group(..)
, mkGroup
, Case(..)
, mkCase
, Loc(..)
, zero
, Semiring(..)
, Unital(..)
, Tropical(..)
, toInt
, H(..)
, V(..)
) where

import Data.Char (isSpace)
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

data Loc = Loc { path :: FilePath, lineNumber :: Int }

mkCase :: String -> Property -> Case
mkCase s property = Case{ name, loc = Loc{ path, lineNumber }, property }
  where
  (name, path, lineNumber) = case breaks [isSpace, not . isSpace, isSpace, not . isSpace, (== ':'), (/= ':')] s of
    [n, _, _, _, p, _, l] -> (unwords (filter (\ s -> s /= "_" && s /= "prop") (breakAll (== '_') n)), p, fst (head (readDec l)))
    _                     -> ("", "", 0)


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


newtype Tropical = Tropical { getTropical :: Maybe Int }
  deriving (Eq, Ord, Show)

instance Semigroup Tropical where
  (<>) = max

instance Monoid Tropical where
  mempty = Tropical Nothing

instance Semiring Tropical where
  Tropical Nothing  >< _                 = Tropical Nothing
  _                 >< Tropical Nothing  = Tropical Nothing
  Tropical (Just a) >< Tropical (Just b) = Tropical (Just (a + b))

instance Unital Tropical where
  one = Tropical (Just 0)

toInt :: Tropical -> Int
toInt = \case
  Tropical Nothing  -> minBound
  Tropical (Just i) -> i


newtype H a = H { getH :: [a] }

instance Semigroup (H a) where
  H a1 <> H a2 = H (a1 <> a2)

instance Monoid (H a) where
  mempty = H []


newtype V a = V { getV :: [a] }

instance Semigroup (V a) where
  V a1 <> V a2 = V (a1 <> a2)

instance Monoid (V a) where
  mempty = V []
