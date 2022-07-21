{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day11 where

import           Perlude

import           Control.Monad        (replicateM)
import           Control.Monad.State  (MonadState, evalState, get, gets, modify)
import           Data.Advent          (Day (..))
import           Data.Bidim           (Bidim, Coord, showBidim, toMap)
import qualified Data.Bidim           as Bidim
import           Data.Foldable        (traverse_)
import           Data.Generics.Labels ()
import qualified Data.HashMap.Strict  as HashMap
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HashSet
import           Data.Maybe           (fromJust)
import           Data.Text            (intercalate)
import qualified Data.Text            as Text
import           System.IO.Advent     (getInput, solve)
import           Text.Parsec.Bidim    (bidim)
import           Text.Parsec.Parselib (Parser, unsafeParseAll)

-- TODO This problem may be solvable just using parsec
type Parsed = Bidim Int

day :: Day
day = D11

rawInput :: IO Text
rawInput = getInput day

example :: Text
example =
  intercalate "\n" [
  "5483143223",
  "2745854711",
  "5264556173",
  "6141336146",
  "6357385478",
  "4167524645",
  "2176841721",
  "6882881134",
  "4846848554",
  "5283751526"
  ]

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parser :: Parser Parsed
parser = bidim (read . Text.singleton)

increaseAllEnergy :: MonadState (Bidim Int) m => m ()
increaseAllEnergy = modify (fmap (+1))

increaseNeighbourEnergy :: MonadState (Bidim Int) m => Coord -> m ()
increaseNeighbourEnergy coord = do
  octopuses <- get
  let toIncrease = Bidim.fromList $ zip (neighbours octopuses coord) (repeat 1)
  modify (Bidim.mergeWith (+) toIncrease)

-- TODO Move to bidim
neighbours :: Bidim Int -> Coord -> [Coord]
neighbours b (x, y) =
  [(x', y')
  | x' <- [x - 1, x, x + 1],
    y' <- [y - 1, y, y + 1],
    (x', y') /= (x, y),
    HashMap.member (x', y') (toMap b)]

findFlashers :: MonadState (Bidim Int) m => m (HashSet Coord)
findFlashers =
  gets (HashSet.fromList . HashMap.keys . HashMap.filter (> 9) . toMap)

propagateToNeighbours :: MonadState (Bidim Int) m => HashSet Coord -> m ()
propagateToNeighbours = traverse_ increaseNeighbourEnergy

-- The flash phase starts with an empty set and returns the set with all
-- positions that flashed
flashPhase :: MonadState (Bidim Int) m => HashSet Coord -> m (HashSet Coord)
flashPhase previousFlashers = do
  flashers <- findFlashers
  if flashers ==  previousFlashers
    then pure flashers
    else do
    propagateToNeighbours $ HashSet.difference flashers previousFlashers
    flashPhase $ HashSet.union flashers previousFlashers

resetEnergy :: MonadState (Bidim Int) m => Coord -> m ()
resetEnergy coord = modify (Bidim.insert coord 0)

-- Returns the amount of flashed cells
step :: MonadState (Bidim Int) m => m Int
step = do
  increaseAllEnergy
  flashed <- flashPhase HashSet.empty
  traverse_ resetEnergy flashed
  pure $ HashSet.size flashed

showEnergy :: Bidim Int -> Text
showEnergy =
  let
    showCell x | x <= 9 = show x
               | otherwise = "*"
  in
  showBidim (showCell . fromJust)

solver1 :: Parsed -> Int
solver1 = sum . evalState (replicateM 100 step)

solver2 :: Parsed -> Int
solver2 input =
  let
    totalAmount = HashMap.size . toMap $ input
  in
    (+1) . length . takeWhile (< totalAmount)
    . evalState (sequence . repeat $ step)
    $ input

main :: IO ()
main = solve day parser solver1 solver2
