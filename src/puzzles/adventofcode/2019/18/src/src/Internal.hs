-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Internal (
  MazeNode (MazeNode),
  MazeMemory (MazeMemory),
  MazeContext,
  initialNode,
  hValueOfKey,
  toMemory
  ) where

import           Control.Lens  (ix, preview)
import           Data.Hashable (Hashable)
import           Data.HashSet  (HashSet, empty)
import           Data.Maybe    (fromJust)
import           GHC.Generics  (Generic)

import           Bidim

type MazeContext = Bidim Char

data MazeNode = MazeNode {
  pos     :: [Coord],
  robotIx :: Int,
  path    :: [[Coord]],
  keys    :: HashSet Char,
  c       :: Int,
  h       :: Int
  } deriving stock (Eq, Generic, Show)

instance Hashable MazeNode

data MazeMemory = MazeMemory {
  robotIx :: Int,
  pos     :: Coord,
  keys    :: HashSet Char
  } deriving stock (Eq, Generic, Show)

instance Hashable MazeMemory

-- This should not overshoot, if we want to guarantee the best solution
hValueOfKey :: Int
hValueOfKey = 1

initialNode :: [Coord] -> Int -> MazeNode
initialNode startingPoints numberOfKeys =
  MazeNode {
  pos = startingPoints,
  path = [],
  robotIx = 0,
  keys = empty,
  c = 0,
  h = numberOfKeys * hValueOfKey
  }

toMemory :: MazeNode -> MazeMemory
toMemory node =
  let movedRobot = robotIx (node :: MazeNode)
  in MazeMemory {
    robotIx = movedRobot,
    pos = fromJust $ preview (ix movedRobot) $ pos (node :: MazeNode),
    keys = keys (node :: MazeNode)
  }
