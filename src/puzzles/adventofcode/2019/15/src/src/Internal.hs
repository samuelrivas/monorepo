-- This module exists mainly to keep accessors from leaking out of records

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedLabels   #-}
module Internal (
  Cell (..),
  Move (..),
  Exploration,
  mkExploration,
  Node,
  mkNode
  ) where

import           Prelude

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence   (Seq)
import qualified Data.Sequence   as Seq
import           GHC.Generics    (Generic)

import           Bidim           (Coord)
import           Intcode         (IntcodeState)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

data Cell = Wall | Empty | Goal
  deriving stock (Show, Eq, Enum)

data Move = North | South | West | East
  deriving stock (Eq, Enum, Bounded, Show)

data Exploration = Exploration {
  map   :: Map Coord Cell,
  nodes :: Seq Node,
  goal  :: Maybe (Coord, [Move])
  } deriving stock (Show, Generic)

mkExploration :: Exploration
mkExploration = Exploration {
  Internal.map = Map.empty,
  nodes = Seq.empty,
  goal = Nothing
  }

data Node = Node {
  move         :: Move,
  pos          :: Coord,
  intcodeState :: IntcodeState,
  path         :: [Move]
  } deriving stock (Show, Generic)

mkNode :: Move -> Coord -> IntcodeState -> [Move] -> Node
mkNode = Node
