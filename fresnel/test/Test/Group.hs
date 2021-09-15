{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
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
, H(..)
, V(..)
, Width(..)
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

mkCase :: String -> Property -> Case
mkCase s property = Case{ name, loc = Loc{ path, lineNumber }, property }
  where
  (name, path, lineNumber) = case breaks [isSpace, not . isSpace, isSpace, not . isSpace, (== ':'), (/= ':')] s of
    [n, _, _, _, p, _, l] -> (unwords (filter (\ s -> s /= "_" && s /= "prop") (breakAll (== '_') n)), p, fst (head (readDec l)))
    _                     -> ("", "", 0)


data Loc = Loc { path :: FilePath, lineNumber :: Int }


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
  one = Tropical (Just 0)


newtype H a = H { getH :: [a] }


newtype V a = V { getV :: [a] }


class Width t where
  width :: t -> Tropical Int

instance Width a => Width (H a) where
  width = foldr ((><) . width) one . getH

instance Width a => Width (V a) where
  width = foldr ((<>) . width) zero . getV

instance Width Char where
  width _ = Tropical (Just 1)

instance Width String where
  width = Tropical . Just . length

instance Width Group where
  width Group{ groupName, cases } = width groupName <> width (V cases)

instance Width Case where
  width Case{ name } = width name
