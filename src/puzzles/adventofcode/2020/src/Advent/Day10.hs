{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day10 where

import           Perlude

import           Data.Advent        (Day (..))
import           Data.List          (sort)
import           Data.Matrix        (Matrix, getElem, matrix, ncols)
import           Data.Set           (member)
import qualified Data.Set           as Set
import           System.IO.Advent   (getInput, solve)
import           Text.Parsec.Advent (listOfNum)
import           Text.Parsec.Text   (Parser)

day :: Day
day = D10

smallExample :: [Int]
smallExample = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]

example :: [Int]
example = [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39,
           11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]

rawInput :: IO Text
rawInput = getInput day

parser :: Parser [Int]
parser = listOfNum

solver1 :: [Int] -> Int
solver1 l =
  let
    sorted = sort (0:l)
    diffs = uncurry (flip (-)) <$> zip sorted (tail sorted)
    ones = length . filter (== 1) $ diffs
    threes = length . filter (== 3) $ diffs
  in
    ones * (threes + 1) -- Adding the built-in adapter here

toAdjMatrix :: [Int] -> Matrix Int
toAdjMatrix adapterList =
  let
    finalAdapter = maximum adapterList + 3
    adapters = Set.fromList (0 : finalAdapter : adapterList)
    adjacent (x', y') =  -- adapt from zero indexing to 1 indexing
      let
        x = x' - 1
        y = y' - 1
      in
           (x `member` adapters)
        && (y `member` adapters)
        && (y - x `elem` [1..3])

  in matrix (finalAdapter + 1) (finalAdapter + 1) (fromEnum . adjacent)

-- TODO: We are building a graph of connectable adaptors and counting all paths
-- af any lenght that connect the first to the last. There is a more direct way
-- to solve this that just requires an array and n^2 calculations
solver2 :: [Int] -> Int
solver2 l =
  let
    m = toAdjMatrix l
    size = ncols m
    nPaths = getElem 1 size . (m ^) <$> [1..size]

  in sum nPaths

main :: IO ()
main = solve day parser solver1 solver2
