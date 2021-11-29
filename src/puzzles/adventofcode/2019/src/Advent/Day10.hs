-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Advent.Day10 where

import           Perlude

import           Control.Lens         (_1, _2, view)
import           Data.Advent          (Day (..))
import           Data.Bidim           (Bidim, Coord, plus)
import           Data.List            (groupBy, sortBy, transpose)
import           Data.Map             (keys)
import qualified Data.Map             as Map
import           Data.Ord             (comparing)
import           Data.Ratio           (Ratio, denominator, numerator, (%))
import           Data.Text            (intercalate)
import qualified Prelude
import           System.IO.Advent     (getInput, solve)
import           Text.Parsec.Bidim    (bidim)
import           Text.Parsec.Parselib (Parser)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
day :: Day
day = D10

example1 :: Text
example1 = intercalate "\n"
  [".#..#",
   ".....",
   "#####",
   "....#",
   "...##"]

example2 :: Text
example2 = intercalate "\n"
  ["......#.#.",
   "#..#.#....",
   "..#######.",
   ".#.#.###..",
   ".#..#.....",
   "..#....#.#",
   "#..#....#.",
   ".##.#..###",
   "##...#..#.",
   ".#....####"]

example3 :: Text
example3 = intercalate "\n"
  [".#....#####...#..",
   "##...##.#####..##",
   "##...#...#.#####.",
   "..#.....X...###..",
   "..#.#.....#....##"]

example4 :: Text
example4 = intercalate "\n"
  [".#..##.###...#######",
   "##.############..##.",
   ".#.######.########.#",
   ".###.#######.####.#.",
   "#####.##.#.##.###.##",
   "..#####..#.#########",
   "####################",
   "#.####....###.#.#.##",
   "##.#################",
   "#####.##.###..####..",
   "..######..##.#######",
   "####.##.####...##..#",
   ".#####..#.######.###",
   "##...#.##########...",
   "#.##########.#######",
   ".####.#.###.###.#.##",
   "....##.##.###..#####",
   ".#.#.###########.###",
   "#.#.#.#####.####.###",
   "###.##.####.##.#..##"]

getRawInput :: IO Text
getRawInput = getInput day

-- Attempt to use Bidim, but is not finished
parser :: Parser (Bidim Bool)
-- parser = fixCoords <$> bidim (== '#')
parser = bidim (== '#')

-- fixCoords :: Bidim Bool -> Bidim Bool
-- fixCoords b =
--   let (_, (_, maxY)) = boundaries b
--   in Map.mapKeys (over _2 (maxY -)) b

-- We divide the plane in Positive, with x => 0 and negative, with x < 0. For
-- each semiplane, we represent slope as the "cosinoid" of any coordinate in a
-- line
data Slope = Positive (Ratio Int)
  | Negative (Ratio Int)
  deriving stock (Eq)

instance Ord Slope where
  compare (Positive _) (Negative _) = GT
  compare (Negative _) (Positive _) = LT
  compare (Positive x) (Positive y) = compare x y
  compare (Negative x) (Negative y) = compare y x

instance Show Slope where
  show (Positive r) = unpack $ show (numerator r) <> "/" <> show (denominator r)
  show (Negative r) = unpack $ "-" <> show (Positive r)

mkSlope :: Coord -> Slope
mkSlope coord@(x, _)
  | x >=0 = Positive $ cosinoid coord
  | otherwise = Negative $ cosinoid coord

-- The actual cosine would be y^2/sqrt(x^2+y^2), but this formula should
-- preserve order, which is what we want, and won't need pesky floats.
--
--- Note that we are reversing the y axes so that positive slopes point upward
--- as the y axis is reversed in the problem (numbers grow larger downwards
cosinoid :: Coord -> Ratio Int
cosinoid (x, y) = (-y) % norm (x, y)

-- TODO Move to library as l1-norm
norm :: Coord -> Int
norm (x, y) = abs x + abs y

-- TODO add sortWith to some library
slopeSort :: [Coord] -> [Coord]
slopeSort = sortBy (comparing mkSlope)

normSort :: [Coord] -> [Coord]
normSort = sortBy (comparing norm)

-- TODO add to library
equating :: Eq a => (b -> a) -> b -> b -> Bool
equating f a b = f a == f b

-- TODO add groupWith to some library
slopeGroup :: [Coord] -> [[Coord]]
slopeGroup = groupBy (equating mkSlope)

-- TODO This is probably better with Multisets

asteroids :: Bidim Bool -> [Coord]
asteroids = keys . Map.filter id

relativize :: Coord -> [Coord] -> [Coord]
relativize (x, y) = fmap (plus (-x, -y))

unRelativize :: Coord -> [Coord] -> [Coord]
unRelativize (x, y) = relativize (-x, -y)

getRelativeAsteroids :: Bidim Bool -> Coord -> [Coord]
getRelativeAsteroids field coord =
  filter (/= (0, 0)) . relativize coord . asteroids $ field

inSight :: Bidim Bool -> Coord -> Int
inSight field coord =
  length . slopeGroup . slopeSort $ getRelativeAsteroids field coord


bestLocation :: Bidim Bool -> (Int, Coord)
bestLocation field =
  let
    candidates = asteroids field
    scored = zip (inSight field <$> candidates) candidates
  in
    maximum scored

-- TODO move to library
interleave :: [[a]] -> [a]
interleave = concat . transpose

-- TODO Clean this up
vaporizationSequence :: Bidim Bool -> Coord -> [Coord]
vaporizationSequence field location =
  let
    positions = getRelativeAsteroids field location
  in
    interleave . reverse $ (fmap (unRelativize location) <$> fmap normSort . slopeGroup . slopeSort $ positions)

solver1 :: Bidim Bool -> Int
solver1 = view _1 . bestLocation

-- TODO We need support in 'solve' to pass information from the first solver to
-- this one
solver2 :: Bidim Bool -> Int
solver2 b =
  let
    best = view _2 $ bestLocation b
    (x, y) = vaporizationSequence b best !! 199
  in
    100 * x + y

main :: IO ()
main = solve day parser solver1 solver2
