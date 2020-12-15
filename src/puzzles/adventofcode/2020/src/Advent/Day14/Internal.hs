{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Advent.Day14.Internal (
  ComputerState (ComputerState),
  Instruction (..),
  mkComputerState
  ) where

import           Advent.Perlude

import           Data.Map       (Map, empty)
import           GHC.Generics   (Generic)

data ComputerState = ComputerState
    { mask   :: Text
    , memory :: Map Int Int
    }
    deriving stock (Generic, Eq, Show)

mkComputerState :: ComputerState
mkComputerState = ComputerState {
  mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
  memory = empty
  }

data Instruction = Mask Text
    | Mem Int Int
    deriving stock (Eq, Show)
