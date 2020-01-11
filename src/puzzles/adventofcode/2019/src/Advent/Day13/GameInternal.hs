-- This module exists mainly to keep accessors from leaking out of records

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedLabels   #-}
module Advent.Day13.GameInternal (
  GameState,
  Tile (..),
  initial_state
  ) where

import           Advent.Day13.Intcode (ComputerState)
import           Data.Generics.Labels ()
import           Data.Map.Strict      (Map, empty)
import           GHC.Generics         (Generic)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

type Coord = (Int, Int)

data Tile = Empty | Wall | Block | Paddle | Ball
  deriving stock (Show, Eq, Enum)

data GameState = GameState {
  screen      :: Map Coord Tile,
  score       :: Integer,
  inputs      :: [Integer],
  saved_state :: ComputerState
  } deriving stock (Generic, Show)

initial_state :: GameState
initial_state = GameState empty 0 [] undefined
