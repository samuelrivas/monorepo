-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Advent.Day10 where

import           Prelude              hiding (getLine, lines, putStrLn,
                                       readFile)

import           Control.Lens         (maximum1Of, modifying, over, set, use,
                                       uses, view, _1, _2)
import           Control.Monad.Loops  (whileM)
import           Control.Monad.State  (State, evalState)
import           Data.Foldable        (sequence_)
import           Data.Generics.Labels ()
import           Data.HashSet         (HashSet, delete, fromList, toList)
import           Data.Ratio           (denominator, numerator, (%))
import           Data.Text            (Text, intercalate, pack)
import qualified Data.Text            as Text
import           Data.Text.IO         (putStrLn)
import           GHC.Generics         (Generic)

import           System.IO.Advent     (getInput)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
type Coord = (Int, Int)

data Acc = Acc {
  asteroids :: HashSet Coord,
  position  :: Coord,
  limit     :: Coord
  } deriving stock (Generic, Show)

example_1 :: Text
example_1 = intercalate "\n"
  [".#..#",
   ".....",
   "#####",
   "....#",
   "...##"]

example_2 :: Text
example_2 = intercalate "\n"
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

-- Return the set of all satellites and the bottom right coordinate
parse :: Text -> (HashSet Coord, Coord)
parse text =
  let
    f (l, x, y) = \case
      '#' -> ((x, y) : l, x + 1, y)
      '.' -> (l, x + 1, y)
      '\n' -> (l, 0, y + 1)
      _ -> error "badmatch"
    coords = view _1 $ Text.foldl' f ([], 0, 0) text
  in
    (fromList coords,
     (maximum1Of (traverse . _1) coords,
      maximum1Of (traverse . _2) coords))

init_acc :: Text -> Acc
init_acc text =
  let (asteroids', limit') = parse text
  in Acc asteroids' (0,0) limit'

set_position :: Acc -> Coord -> Acc
set_position acc pos =
  set #position pos $
  over #asteroids (delete pos) acc

-- Given to coordinates, return an infinite list of all coordinates starting
-- from the first and in line with the second
trace :: Coord -> Coord -> [Coord]
trace a@(x1, y1) b =
  let
    (x_step, y_step) = slope a b
  in
    zip (iterate (+ x_step) x1) (iterate (+ y_step) y1)

slope :: Coord -> Coord -> Coord
slope (x1, y1) (x2, y2) =
  let
    slope' = abs (x2 - x1) % abs (y2 - y1)
    x_sign = if x2 > x1 then 1 else (-1)
    y_sign = if y2 > y1 then 1 else (-1)
  in
    if y1 == y2
    then (x_sign, 0)
    else (x_sign * numerator slope', y_sign * denominator slope')

in_limits :: Coord -> [Coord] -> [Coord]
in_limits (max_x, max_y) =
  let valid (x, y) = x <= max_x && y <= max_y && x >= 0 && y >= 0
  in takeWhile valid

-- Remove one line of asteroids
step :: State Acc ()
step = do
  asteroid <- uses #asteroids (head . toList)
  origin <- use #position
  limit' <- use #limit
  let ray = in_limits limit' $ trace origin asteroid
  sequence_ (modifying #asteroids . delete <$> ray)

count_hits :: State Acc Int
count_hits = length <$> whileM (uses #asteroids $ not . null) step

get_hits :: Acc -> Coord -> Int
get_hits acc pos = evalState count_hits $ set_position acc pos

solution_1 :: Text -> Int
solution_1 input =
  let
    acc = init_acc input
  in
    maximum $ get_hits acc <$> toList (view #asteroids acc)

main :: IO ()
main = do
  input <- getInput "10"
  putStrLn $ "Solution 1: " <> (pack . show $ solution_1 input)
  putStrLn "Solution 2: not done :("
  
