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
import           Data.Maybe       (fromJust, isJust)
import           Data.Set         (Set)
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

-- must run on sorted lists, current starts at initial joltage (0) This groups
-- the possible adapters at every step, taking the latest adapter from the
-- previous step
options :: Int -> [Int] -> [[Int]]
options _ [] = []
options current l =
  let (candidates, rest) = span (< (current + 4)) l
  in case candidates of
    [] -> []
    _  -> candidates : options (last candidates) rest


toMultiplier :: [Int] -> Int
toMultiplier [_]       = 1
toMultiplier [_, _]    = 2
toMultiplier [_, _, _] = 4
toMultiplier _         = undefined

-- On sorted lists, x smaller than head
-- which adapter list to check next
next :: Int -> [Int] -> [[Int]]
next x = takeWhile ((<= x + 3) . head) . filter ((>= 2) . length) . tails

countSolutions :: [Int] -> Int
countSolutions [] = 1
countSolutions (h:t) =
  let
    possible = next h t
  in
    if null possible then 1
    else sum $ countSolutions <$>possible

main :: IO ()
main = do
  input <- parse <$> getInput

  putStr "Solution 1: "
  print . solution1 $ input

  putStr "Solution 2: "
  print $ "NA"
