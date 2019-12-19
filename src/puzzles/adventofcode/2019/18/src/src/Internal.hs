{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Internal (
  MazeNode (MazeNode),
  MazeContext,
  initialNode
  ) where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.Monad.State (StateT)
import Data.HashSet (HashSet, empty)

import Bidim

type MazeContext = Bidim Char

data MazeNode = MazeNode {
  pos :: Coord,
  path :: [Coord],
  keys :: HashSet Char,
  c :: Int,
  h :: Int
  } deriving stock (Eq, Generic, Show)

instance Hashable MazeNode

initialNode :: Coord -> Int -> MazeNode
initialNode startingPoint numberOfKeys =
  MazeNode {
  pos = startingPoint,
  path = [],
  keys = empty,
  c = 0,
  h = numberOfKeys
  }
  
