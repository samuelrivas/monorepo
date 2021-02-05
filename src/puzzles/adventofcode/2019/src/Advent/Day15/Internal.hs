-- This module exists mainly to keep accessors from leaking out of records

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedLabels   #-}
module Advent.Day15.Internal (
  Cell (..),
  Move (..),
  Exploration,
  mkExploration,
  Node,
  mkNode
  ) where

import           Prelude

import           Data.Bidim           (Coord)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Sequence        (Seq)
import qualified Data.Sequence        as Seq
import           GHC.Generics         (Generic)

import           Advent.Day15.Intcode (IntcodeState)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

data Cell = Wall
    | Empty
    | Goal
    deriving stock (Show, Eq, Enum)

data Move = North
    | South
    | West
    | East
    deriving stock (Eq, Enum, Bounded, Show)

data Exploration = Exploration
    { map      :: Map Coord Cell
    , nodes    :: Seq Node
    , goal     :: Maybe (Coord, [Move])
    , goalNode :: Maybe Node
    , maxPath  :: Integer
    }
    deriving stock (Show, Generic)

mkExploration :: Exploration
mkExploration = Exploration {
  Advent.Day15.Internal.map = Map.empty,
  nodes = Seq.empty,
  goal = Nothing,
  goalNode = Nothing,
  maxPath = 0
  }

data Node = Node
    { move         :: Move
    , pos          :: Coord
    , intcodeState :: IntcodeState
    , path         :: [Move]
    }
    deriving stock (Show, Generic)

mkNode :: Move -> Coord -> IntcodeState -> [Move] -> Node
mkNode = Node
