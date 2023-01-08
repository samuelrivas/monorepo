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
{-# LANGUAGE TupleSections         #-}
module Advent.Day11 where

import           Prelude               hiding (Left, Right, concat, getLine,
                                        putStrLn, readFile, show)

import           Control.Lens          (_1, _2, _3, assign, at, modifying, non,
                                        over, set, toListOf, use, view)
import           Control.Monad.Loops   (whileM_)
import           Control.Monad.State   (StateT, lift, runStateT)
import           Data.Functor.Identity (runIdentity)
import           Data.Generics.Labels  ()
import           Data.Map.Strict       (Map, empty, keys, size)
import           Data.Text             (Text, concat, intercalate, splitOn,
                                        unpack)
import           Data.Text.IO          (putStrLn)
import           GHC.Generics          (Generic)

import           Advent.Day11.Intcode  hiding (initial_state)
import           Advent.Day11.Internal hiding (initial_state)
import           Data.Advent           (Day (..))
import           System.IO.Advent      (getInput)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

data Colour = Black | White
  deriving stock (Show, Eq, Enum)

data Rotate = Counter | Clock
  deriving stock (Show, Eq, Enum)

data Direction = Up | Right | Down | Left
  deriving stock (Show, Eq)

encode :: (Num c, Enum a) => a -> c
encode = fromIntegral . fromEnum

decode :: (Integral c, Enum a) => c -> a
decode = toEnum . fromIntegral

type Coord = (Int, Int)

data RobotState = RobotState {
  panels    :: Map Coord Colour,
  pos       :: Coord,
  direction :: Direction
  } deriving stock (Show, Generic)

type RobotT m = IntcodeT (StateT RobotState m)

initial_state :: RobotState
initial_state = RobotState {
  panels = empty,
  pos = (0, 0),
  direction = Up
  }

initial_state_2 :: RobotState
initial_state_2 = set (#panels . at (0, 0)) (Just White) initial_state

rotate :: Rotate -> Direction -> Direction
rotate Counter Up    = Left
rotate Counter Left  = Down
rotate Counter Down  = Right
rotate Counter Right = Up
rotate Clock Up      = Right
rotate Clock Right   = Down
rotate Clock Down    = Left
rotate Clock Left    = Up

next_pos :: Direction -> Coord -> Coord
next_pos Up    = over _2 (+ 1)
next_pos Down  = over _2 (+ (-1))
next_pos Left  = over _1 (+ (-1))
next_pos Right = over _1 (+ 1)

get_move :: Monad m => RobotT m (Rotate, Colour)
get_move = do
  location <- lift $ use #pos
  on_colour <- lift $ use (#panels . at location . non Black)
  push_input [encode on_colour]
  run_program
  get_output >>= \case
    [mv, colour] -> do
      flush_output
      pure  (decode mv, decode colour)
    _ -> do
      abort "Didn't get two outputs!"
      pure (Counter, Black) -- FIXME!!

paint :: Monad m => Colour ->  RobotT m ()
paint colour = do
  location <- lift $ use #pos
  trace $ "Painting " <> show location <> " " <> show colour
  lift $ assign (#panels . at location) (Just colour)

move :: Monad m => Rotate -> RobotT m ()
move rotation = do
  lift $ modifying #direction (rotate rotation)
  dir <- lift $ use #direction
  trace $ "Setting direction " <> show dir
  lift $ modifying #pos (next_pos dir)

step :: Monad m => RobotT m ()
step = do
  location <- lift $ use #pos
  trace $ "Location: " <> show location
  (rotation, colour) <- get_move
  paint colour
  move rotation

loop :: Monad m => RobotT m ()
loop = whileM_ ((== Interrupted) <$> get_status) step

get_input :: IO [Integer]
get_input = fmap (read . unpack) . splitOn "," <$> getInput D11

run_robot ::
  Monad m =>
  RobotT m a -> RobotState -> [Integer] -> m (a, Status, RobotState, Text)
run_robot x in_state code = do
  ((a, comp_state, w), out_state) <- runStateT (launch x code) in_state
  pure (a, view #status comp_state, out_state, w)

solution_1 :: [Integer] -> Int
solution_1 code =
  let
    robot_state = view _3 . runIdentity $
      run_robot (step >> loop) initial_state code
  in
    size . view #panels $ robot_state

show_map :: (Maybe a -> Text) -> Map Coord a -> Text
show_map format plane =
  let
    coords = keys plane
    xs = toListOf (traverse . _1) coords
    ys = toListOf (traverse . _2) coords
    max_x = maximum xs
    min_x = minimum xs
    max_y = maximum ys
    min_y = minimum ys
    row y = (, y) <$> [min_x..max_x]
    show_coord coord = format $ view (at coord) plane
    printed y = concat (show_coord <$> row y)
  in
    intercalate "\n" (printed <$> reverse [min_y..max_y])

formatter :: Maybe Colour -> Text
formatter (Just White) = "X"
formatter (Just Black) = " "
formatter Nothing      = "."

solution_2 :: [Integer] -> Text
solution_2 code =
  let
    painted = view (_3 . #panels) . runIdentity $
      run_robot (step >> loop) initial_state_2 code
  in
    show_map formatter painted

main :: IO ()
main = do
  code <- get_input
  putStrLn $ "Solution 1: " <> show (solution_1 code)
  putStrLn "Solution 2: "
  putStrLn $ solution_2 code
