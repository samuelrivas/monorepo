{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import           Prelude               hiding (getLine, putStrLn, readFile, Left, Right, show)

import           Control.Lens          (use, non, view, _3, at, assign, modifying, over, _1, _2)
import           Data.Functor.Identity (runIdentity)
import           Data.Generics.Labels  ()
import           Data.Text             (Text, splitOn, unpack, pack)
import           Data.Text.IO          (putStrLn, readFile)
import Control.Monad.State (State, StateT, lift, runStateT)
import Data.Map.Strict (Map, empty)
import GHC.Generics (Generic)

import           Intcode
import           Internal

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
  panels :: Map Coord Colour,
  pos :: Coord,
  direction :: Direction
  } deriving stock (Show, Generic)

type RobotT m = ProgramT (StateT RobotState m)

initial_state :: RobotState
initial_state = RobotState {
  panels = empty,
  pos = (0, 0),
  direction = Up
  }

rotate :: Rotate -> Direction -> Direction
rotate Counter Up = Left
rotate Counter Left = Down
rotate Counter Down = Right
rotate Counter Right = Up
rotate Clock Up = Right
rotate Clock Right = Down
rotate Clock Down = Left
rotate Clock Left = Up

next_pos :: Direction -> Coord -> Coord
next_pos Up = over _2 (+ 1)
next_pos Down = over _2 (+ (-1))
next_pos Left = over _1 (+ (-1))
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

get_input :: IO [Integer]
get_input = fmap (read . unpack) . splitOn "," <$> readFile "input.txt"

run_robot ::
  Monad m =>
  RobotT m a -> RobotState -> [Integer] -> m (a, Status, RobotState, Text)
run_robot x in_state code = do
  ((a, comp_state, w), out_state) <- runStateT (launch x code) in_state
  pure (a, view #status comp_state, out_state, w)

main :: IO ()
main = do
  code <- get_input
  undefined
