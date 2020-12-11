{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day11 where

import           Advent.Perlude

import           Control.Lens     (at, both, each, foldlOf, over, preview, set,
                                   view, _1, _2)
import           Control.Monad    (guard)
import           Data.Bidim       (Bidim, Coord, plus)
import qualified Data.Bidim       as Bidim
import           Data.List        (find, foldl', sort, unfoldr)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe       (catMaybes, fromJust, isJust)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import qualified Data.Text        as Text
import qualified System.IO.Advent as IOAdvent

-- TODO: There is a latent abstraction here. Many advent puzzles consist of a
-- state and rules to move to the next state. And many are based on maps and
-- rules of adjacency
example :: Text
example = "L.LL.LL.LL\n\
          \LLLLLLL.LL\n\
          \L.L.L..L..\n\
          \LLLL.LL.LL\n\
          \L.LL.LL.LL\n\
          \L.LLLLL.LL\n\
          \..L.L.....\n\
          \LLLLLLLLLL\n\
          \L.LLLLLL.L\n\
          \L.LLLLL.LL\n"

getInput :: IO Text
getInput = IOAdvent.getInput "11"

parse :: Text -> Bidim Char
parse = Bidim.fromText

-- TODO: Move to Bidim
around :: Coord -> [Coord]
around coord = [
  coord `plus` (-1, -1),
  coord `plus` (-1, 0),
  coord `plus` (-1, 1),
  coord `plus` (0, -1),
  coord `plus` (0, 1),
  coord `plus` (1, -1),
  coord `plus` (1, 0),
  coord `plus` (1, 1)
  ]

upRay :: Coord -> [Coord]
upRay = iterate $ plus (0, 1)

downRay :: Coord -> [Coord]
downRay = iterate $ plus (0, -1)

leftRay :: Coord -> [Coord]
leftRay = iterate $ plus (-1, 0)

rightRay :: Coord -> [Coord]
rightRay = iterate $ plus (1, 0)

upLeftRay :: Coord -> [Coord]
upLeftRay = iterate $ plus (-1, 1)

upRightRay :: Coord -> [Coord]
upRightRay = iterate $ plus (1, 1)

downLeftRay :: Coord -> [Coord]
downLeftRay = iterate $ plus (-1, -1)

downRightRay :: Coord -> [Coord]
downRightRay = iterate $ plus (-1, 1)

-- TODO: This can be done more directly with better lenses
getAll :: Char -> Bidim Char -> [Coord]
getAll c = fmap (view _1) . filter ((== c) . view _2) . Map.assocs

-- TODO: This can be done more directly with better lenses
numOccupied :: Bidim Char -> Coord -> Int
numOccupied bidim position =
  let neighbours = (\pos -> view (at pos) bidim) <$> around position
  in length . filter (== Just '#') $ neighbours

updateOccupied :: Int -> Char
updateOccupied neighbours
  | neighbours >= 4 = 'L'
  | otherwise = '#'

updateEmpty :: Int -> Char
updateEmpty neighbours
  | neighbours == 0 = '#'
  | otherwise = 'L'

nextState :: Bidim Char -> Coord -> Char
nextState bidim pos =
  let
    occupied = numOccupied bidim pos
  in
    case view (at pos) bidim of
      Just '#' -> updateOccupied occupied
      Just 'L' -> updateEmpty occupied
      Just '.' -> '.'
      _        -> undefined

step :: Bidim Char -> Bidim Char
step bidim = Map.mapWithKey (\pos _ -> nextState bidim pos) bidim

findStable :: Bidim Char -> Bidim Char
findStable bidim =
  let
    unfolded = iterate step bidim
  in view _1 . fromJust . find (uncurry (==)) $ zip unfolded (tail unfolded)

solution1 :: Bidim Char -> Int
solution1 = length . getAll '#' . findStable

main :: IO ()
main = do
  input <- parse <$> getInput

  putStr "Solution 1: "
  print $ solution1 input

  putStr "Solution 2: "
  print $ "NA"
