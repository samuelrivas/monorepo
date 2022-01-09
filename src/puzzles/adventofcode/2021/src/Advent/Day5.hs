{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day5 where

import           Perlude

import           Advent.Templib       (linesOf)

import           Control.Lens         (_2, filtered, lengthOf, non, view)
import           Control.Monad.State  (MonadState, evalState, gets, modify)
import           Data.Advent          (Day (..))
import           Data.Bidim           (Coord, showBidim)
import           Data.Foldable        (traverse_)
import qualified Data.Map             as Map
import           Data.Maybe           (fromJust)
import           Data.MultiSet        (MultiSet)
import qualified Data.MultiSet        as MultiSet
import           Data.Ratio           (denominator, numerator, (%))
import           Data.Text            (intercalate)
import           System.IO.Advent     (getInput, solve)
import           Text.Parsec          (char)
import           Text.Parsec.Parselib (Parser, digitsAsNum, literal,
                                       unsafeParseAll)

type Parsed = [(Coord, Coord)]

day :: Day
day = D5

rawInput :: IO Text
rawInput = getInput day

example :: Text
example = intercalate "\n" [
  "0,9 -> 5,9",
  "8,0 -> 0,8",
  "9,4 -> 3,4",
  "2,2 -> 2,1",
  "7,0 -> 7,4",
  "6,4 -> 2,0",
  "0,9 -> 2,9",
  "3,4 -> 1,4",
  "0,0 -> 8,8",
  "5,5 -> 8,2"
  ]

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parser :: Parser Parsed
parser = linesOf line

line :: Parser (Coord, Coord)
line =
  (,)
  <$> coord <* literal " -> "
  <*> coord

-- TODO probably good for adventlib
coord :: Parser Coord
coord = (,) <$> digitsAsNum <* char ',' <*> digitsAsNum

-- TODO This is probably good for adventlib (and can most likely be cleaner, if
-- you use gcd to figure out the increments for x and y

-- Note that it is overkill for this problem, as we need only cover horizontal,
-- vertical and diagonal segments, not arbitrary segments
segmentCoords :: (Coord, Coord) -> [Coord]
segmentCoords ((x1, y1), (x2, y2))
  | x1 == x2 && y1 < y2 = zip (repeat x1) [y1..y2]
  | x1 >= x2 = segmentCoords ((x2, y2), (x1, y1))
  | otherwise =
    let
      slope = (y2 - y1) % (x2 - x1)
      bias = y1 % 1 - slope * (x1 % 1)
      approxToInt r = numerator r `div` denominator r
    in
      zip [x1..x2] $
      approxToInt . (+ bias) . (* slope) . (% 1) <$> [x1..x2]

addSegment :: MonadState (MultiSet Coord) m => (Coord, Coord) -> m ()
addSegment  = traverse_ (modify . MultiSet.insert) . segmentCoords

hAndV :: [(Coord, Coord)] -> [(Coord, Coord)]
hAndV = filter $ \((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2

solver1 :: Parsed -> Int
solver1 = solver2 . hAndV

getActivity :: [(Coord, Coord)] -> [(Coord, Int)]
getActivity ends =
  flip evalState MultiSet.empty $ do
  traverse_ addSegment ends
  gets MultiSet.toOccurList

showActivity :: [(Coord, Coord)] -> Text
showActivity =
  let showCell = view (non ".") . fmap show
  in showBidim showCell . Map.fromList . getActivity

solver2 :: Parsed -> Int
solver2 = lengthOf (traverse . _2 . filtered (>= 2)) . getActivity

main :: IO ()
main = solve day parser solver1 solver2
