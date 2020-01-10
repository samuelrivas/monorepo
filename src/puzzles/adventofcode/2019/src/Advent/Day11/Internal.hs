-- This module exists mainly to keep accessors from leaking out of records

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedLabels   #-}
module Advent.Day11.Internal (
  Status (..),
  ComputerState (ComputerState),
  Opcode (..),
  Mode (..),
  initial_state,
  show
  ) where

import           Prelude              hiding (show)
import qualified Prelude

import           Data.Generics.Labels ()
import           Data.Map.Strict      (Map, fromList)
import           Data.Text            (Text, pack)
import           GHC.Generics         (Generic)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

show :: Show a => a -> Text
show = pack . Prelude.show

data Status = Running | Finished | Aborted | Interrupted
  deriving stock (Show, Eq)

data Opcode = Add (Mode, Mode, Mode)
            | Mul (Mode, Mode, Mode)
            | In Mode
            | Out Mode
            | JumpTrue (Mode, Mode)
            | JumpFalse (Mode, Mode)
            | LessThan (Mode, Mode, Mode)
            | Equals (Mode, Mode, Mode)
            | AdjustBase Mode
            | Halt
  deriving stock Show

data Mode = Position | Immediate | Relative
  deriving stock Show

data ComputerState = ComputerState {
  input  :: [Integer],
  output :: [Integer],
  status :: Status,
  pp     :: Integer,
  memory :: Map Integer Integer,
  base   :: Integer
  } deriving stock (Generic, Show)

initial_state :: [Integer] -> ComputerState
initial_state code = ComputerState {
  input = [ ],
  output = [ ],
  status = Running,
  pp = 0,
  memory = fromList (zip [0..] code),
  base = 0
  }
