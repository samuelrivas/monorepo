{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day10 where

import           Advent.Perlude

import           Control.Lens     (at, both, each, foldlOf, over, view, _2)
import           Control.Monad    (guard)
import           Data.List        (find, foldl', sort, tails, unfoldr)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Matrix      (Matrix, getElem, matrix, ncols)
import           Data.Maybe       (fromJust, isJust)
import           Data.Set         (Set, member)
import qualified Data.Set         as Set
import qualified Data.Text        as Text
import qualified System.IO.Advent as IOAdvent

smallExample :: [Int]
smallExample = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]

example :: [Int]
example = [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39,
           11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]

getInput :: IO Text
getInput = IOAdvent.getInput "10"

-- TODO: This is a common parser, move to Adventlib
parse :: Text -> [Int]
parse = fmap read . Text.lines

solution1 :: [Int] -> Int
solution1 l =
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
solution2 :: [Int] -> Int
solution2 l =
  let
    m = toAdjMatrix l
    size = ncols m
    nPaths = getElem 1 size . (m ^) <$> [1..size]

  in sum nPaths

main :: IO ()
main = do
  input <- parse <$> getInput

  putStr "Solution 1: "
  print . solution1 $ input

  putStr "Solution 2: "
  print . solution2 $ input
