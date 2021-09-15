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

class Monoid s => Semiring s where
  one :: s
  (><) :: s -> s -> s
  infixr 7 ><
