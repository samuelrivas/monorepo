-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day3 where

import           Prelude       hiding (getLine)

import           Control.Lens  (over, view, _1, _2, _3)
import           Data.Foldable (concatMap, foldl')
import           Data.Map      (Map, empty, insert, insertWith, keysSet, (!))
import           Data.Set      (Set, intersection)
import qualified Data.Set      as Set
import           Data.Text     (Text, splitOn, unpack)
import qualified Data.Text     as Text
import           Data.Text.IO  (getLine)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

data Move = U | D | R | L
  deriving stock Show

type Grid = Map (Int, Int) Bool

-- Each position is keyed on the coordinates, the value is the minimum cost of
-- getting there from the origin cell
type Grid2 = Map (Int, Int) Int
type Pos = (Int, Int)

example_easy :: (Text, Text)
example_easy = ("R8,U5,L5,D3",
                "U7,R6,D4,L4")

example_1 :: (Text, Text)
example_1 = ("R75,D30,R83,U83,L12,D49,R71,U7,L72",
             "U62,R66,U55,R34,D71,R55,D58,R83")

example_2 :: (Text, Text)
example_2 = ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
             "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

parse :: Text -> Maybe [(Move, Int)]
parse input =
  let codes = (,) <$> Text.head <*> Text.tail <$> splitOn "," input
  in sequence $ parse_code <$> codes

expand :: [(Move, Int)] -> [Move]
expand = concatMap (\(m,d) -> replicate d m)

parse_code :: (Char, Text) -> Maybe (Move, Int)
parse_code (m, u) = do
    move <- case m of
              'R' -> Just R
              'L' -> Just L
              'D' -> Just D
              'U' -> Just U
              _   -> Nothing
    pure (move, read . unpack $ u)

get_new_pos :: (Move, Int) -> Pos -> Pos
get_new_pos (move, distance) =
  case move of
    U -> over _2 (+ distance)
    D -> over _2 (+ (-distance))
    R -> over _1 (+ distance)
    L -> over _1 (+ (-distance))

lay_cable :: [Move] -> Grid
lay_cable =
  let
    step (pos, grid) move =
      let newpos = get_new_pos (move, 1) pos
      in (newpos, insert newpos True grid)
  in
    snd . foldl' step ((0, 0), empty)

lay_cable_2 :: [Move] -> Grid2
lay_cable_2 =
  let
    step (count, pos, grid) move =
      let newpos = get_new_pos (move, 1) pos
      in (count + 1, newpos, insertWith min  newpos count grid)
  in
    view _3 . foldl' step (1, (0, 0), empty)

get_crosses :: Map Pos v -> Map Pos v -> Set Pos
get_crosses x y = intersection (keysSet x) (keysSet y)

manhattan :: Pos -> Pos -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

cross_cables :: [(Move, Int)] -> [(Move, Int)] -> Set Pos
cross_cables x y =
  let
    cable_x = lay_cable . expand $ x
    cable_y = lay_cable . expand $ y
  in
    get_crosses cable_x cable_y

-- Return a set with "travel cost" and the intersection points
cross_cables_2 :: [(Move, Int)] -> [(Move, Int)] -> Set (Int, Pos)
cross_cables_2 x y =
  let
    cable_x = lay_cable_2 . expand $ x
    cable_y = lay_cable_2 . expand $ y
    cost pos = (cable_x ! pos) + (cable_y ! pos)
  in
    Set.map ((,) <$> cost <*> id) $ get_crosses cable_x cable_y

find_solution_1 :: [(Move, Int)] -> [(Move, Int)] -> Int
find_solution_1 x y =
  Set.findMin . Set.map (manhattan (0, 0)) $ cross_cables x y

find_solution_2 :: [(Move, Int)] -> [(Move, Int)] -> (Int, Pos)
find_solution_2 x y =
  Set.findMin $ cross_cables_2 x y

main :: IO ()
main = do
  Just x <- parse <$> getLine
  Just y <- parse <$> getLine
  putStrLn $ "Solution 1: " <> show (find_solution_1 x y)
  putStrLn $ "Solution 2: " <> show (fst $ find_solution_2 x y)
