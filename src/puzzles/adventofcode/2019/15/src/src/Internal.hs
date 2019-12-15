-- This module exists mainly to keep accessors from leaking out of records

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedLabels   #-}
module Internal (
  Cell (..),
  Move (..),
  Exploration,
  Node
  ) where

import           Prelude

import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import GHC.Generics (Generic)

import Bidim (Coord)
import Intcode (IntcodeState)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

data Cell = Wall | Empty | Goal
  deriving stock (Show, Eq, Enum)

data Move = North | South | West | East
  deriving stock (Eq, Enum, Bounded, Show)

data Exploration = Exploration {
  map :: Map Coord Cell,
  nodes :: Seq Node
  } deriving stock (Show, Generic)

data Node = Node {
  move :: Move,
  pos :: Coord,
  intcodeState :: IntcodeState,
  path :: [Move]
  } deriving stock (Show, Generic)
