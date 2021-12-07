{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day4 where

import           Perlude

import           Advent.Templib       (binToDec, linesOf, matrix)

import           Control.Lens         (_1, _2, _Just, allOf, at, folded, non,
                                       none, over, toListOf, use, view)
import           Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import           Control.Monad.State  (MonadState, StateT, modify, runStateT)
import           Data.Advent          (Day (..))
import           Data.Bidim           (Bidim)
import           Data.Foldable        (foldl', traverse_)
import           Data.Functor         (($>))
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HashMap
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HashSet
import           Data.List            (transpose)
import           Data.Map.Strict      (Map)
import           Data.Maybe           (fromJust, isJust)
import           Data.Text            (intercalate)
import           System.IO.Advent     (getInput, solve)
import           Text.Parsec          (char, many1, sepBy, sepEndBy, (<|>))
import           Text.Parsec.Bidim    (bidim)
import           Text.Parsec.Parselib (Parser, digitsAsNum, literal,
                                       unsafeParseAll)

type Parsed =  ([Int], [[[Int]]])

-- Row, Column, Board
type Coord = (Int, Int, Int)

day :: Day
day = D4

rawInput :: IO Text
rawInput = getInput day

example :: Text
example = intercalate "\n" [
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
  "",
  "22 13 17 11  0",
  " 8  2 23  4 24",
  "21  9 14 16  7",
  " 6 10  3 18  5",
  " 1 12 20 15 19",
  "",
  " 3 15  0  2 22",
  " 9 18 13 17  5",
  "19  8  7 25 23",
  "20 11 10 24  4",
  "14 21 16 12  6",
  "",
  "14 21 17 24  4",
  "10 16 15  9 19",
  "18  8 23 26 20",
  "22 11 13  6  5",
  " 2  0 12  3  7"
  ]

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parser :: Parser Parsed
parser =
  (,)
  <$> digitsAsNum `sepBy` char ',' <* literal "\n\n"
  <*> linesOf (matrix digitsAsNum)

dimmensions :: [[[Int]]] -> Coord
dimmensions l =
    (length . head . head $ l,  length . head $ l, length l)

-- TODO Move to library
coordinates :: Coord ->  [Coord]
coordinates (x, y, z) =
  let
    count = [0..]
  in
    zip3
    ((`mod` x) <$> count)
    ((`mod` y) . (`div` x) <$> count)
    ((`mod` z) . (`div` (y * x)) <$> count)

type BoardIndex = HashMap Int (HashSet Coord)

-- Map numbers to the bard, row and column they show up at
boardIndex :: [[[Int]]] -> BoardIndex
boardIndex boards =
  let
    dims = dimmensions boards
    withCoordinates =
      zip (toListOf (traverse . traverse . traverse) boards) (coordinates dims)
  in
    foldl' (\i (x, pos) -> HashMap.insertWith HashSet.union x pos i)
    HashMap.empty (over _2 HashSet.singleton <$> withCoordinates)

type PunchCard = HashSet Coord

punch :: MonadState PunchCard m => Coord -> m ()
punch coord = modify $ HashSet.insert coord

-- TODO I am hardcoding the dimmensions here, which sucks and is wrong to boot
-- Also the (: []) isn't great either, this may be more readable using some
-- lensy fold/traversal
row :: Coord -> [Coord]
row = (over _1 . const <$> [0..4] <*>) . (: [])

column :: Coord -> [Coord]
column = (over _2 . const <$> [0..4] <*>) . (: [])

-- Returns the punched holes
drawNumber ::
  MonadState PunchCard m =>
  MonadReader BoardIndex m =>
  Int -> m (HashSet Coord)
drawNumber n = do
  coords <- view (at n . non HashSet.empty)
  traverse_ punch (HashSet.toList coords)
  pure coords

checkWin :: MonadState PunchCard m =>
  Coord -> m (Maybe [Coord])
checkWin coord =
  let
    rowCoords = row coord
    columnCoords = column coord
  in do
    hasColumn <- all isJust <$> traverse (use . at) columnCoords
    hasRow <- all isJust <$> traverse (use . at) rowCoords
    if hasColumn then
      pure . Just $ columnCoords
    else if hasRow then
      pure . Just $ rowCoords
    else
      pure Nothing

drawAndCheck ::
  MonadState PunchCard m =>
  MonadReader BoardIndex m =>
  Int -> m [[Coord]]
drawAndCheck n = do
  punchedCoords <- drawNumber n
  toListOf (folded . _Just) <$>
    traverse checkWin (HashSet.toList punchedCoords)

runBingo :: BoardIndex -> ReaderT BoardIndex (StateT PunchCard m) a -> m (a, PunchCard)
runBingo index x = runStateT (runReaderT x index) HashSet.empty

solver1 :: Parsed -> Int
solver1 l = undefined

solver2 :: Parsed -> Int
solver2 l = undefined

main :: IO ()
main = solve day parser solver1 solver2
