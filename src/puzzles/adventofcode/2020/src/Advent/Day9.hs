{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Advent.Day9 where

import           Perlude

import           Control.Lens     (_2, at, both, each, foldlOf, over, view)
import           Control.Monad    (fmap, guard)
import           Data.Advent      (Day (..))
import           Data.IntMultiSet (IntMultiSet)
import qualified Data.IntMultiSet as IntMultiSet
import           Data.List        (filter, find, maximum, minimum, splitAt,
                                   tails, take)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe       (fromJust, isJust)
import qualified Data.Set         as Set
import qualified Data.Text        as Text
import qualified System.IO.Advent as IOAdvent

getInput :: IO Text
getInput = IOAdvent.getInput D9

parse :: Text -> [Int]
parse = fmap read . Text.lines

example :: [Int]
example =
  [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117,
   150, 182, 127, 219, 299, 277, 309, 576]

chunkize :: Int -> [Int] -> [[Int]]
chunkize size l = filter ((== size) . length) $ take size <$> tails l

-- This is obviously inefficient, but if it works for the input, is simple enough
findMatching :: Int -> [Int] -> Maybe (Int, Int)
findMatching _target [] = Nothing
findMatching target (h:t) =
  case find ((== target) . (h +)) t of
    Just x  -> Just (h, x)
    Nothing -> findMatching target t


validateChunk :: [Int] -> Bool
validateChunk [] = False
validateChunk [_] = False
validateChunk l =
  let
    (preamble, [value]) = splitAt (length l - 1) l
  in
    case findMatching value preamble of
      Just _  -> True
      Nothing -> False

findNumber :: Int -> [Int] -> Maybe Int
findNumber preambleSize =
  fmap last . find (not . validateChunk) . chunkize (preambleSize + 1)

hasPrefix :: Int -> [Int] -> Maybe [Int]
hasPrefix 0 _ = Just []
hasPrefix _ [] = Nothing
hasPrefix n (h:t) =
  if n < 0 then Nothing
  else (h:) <$> hasPrefix (n  - h) t

findSequence :: Int -> [Int] -> Maybe [Int]
findSequence _ [] = Nothing
findSequence target l =
  case hasPrefix target l of
    Nothing -> findSequence target $ tail l
    Just s  -> Just s

solution2 :: Int -> [Int] -> Maybe Int
solution2 preambleSize input = do
  target <- findNumber preambleSize input
  match <- findSequence target input
  pure $ maximum match + minimum match

main :: IO ()
main = do
  input <- parse <$> getInput

  putStr "Solution 1: "
  print . fromJust $ findNumber 25 input

  putStr "Solution 2: "
  print . fromJust . solution2 25 $ input
