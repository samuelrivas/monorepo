{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Advent.Day14.Internal (
  ComputerState (ComputerState),
  Instruction (..),
  Trit (..),
  mkComputerState
  ) where

import           Advent.Perlude

import           Data.Map       (Map, empty)
import           GHC.Generics   (Generic)

data Trit = I
    | O
    | X
    deriving stock (Show, Eq)

data ComputerState = ComputerState
    { mask   :: [Trit]
    , memory :: Map Int Int
    }
    deriving stock (Generic, Eq, Show)

mkComputerState :: ComputerState
mkComputerState = ComputerState {
  mask = replicate 36 X,
  memory = empty
  }

data Instruction = Mask [Trit]
    | Mem Int Int
    deriving stock (Eq, Show)
