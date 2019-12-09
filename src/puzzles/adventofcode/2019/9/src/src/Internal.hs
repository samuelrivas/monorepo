-- This module exists mainly to keep accessors from leaking out of records

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedLabels   #-}
module Internal (
  Status (..),
  ComputerState (ComputerState),
  Opcode (..),
  Mode (..),
  initial_state
  ) where

import           Data.Array           (Array, listArray)
import           Data.Generics.Labels ()
import           GHC.Generics         (Generic)

{-# ANN module "HLint: ignore Use camelCase" #-}

data Status = Running | Finished | Aborted | Interrupted
  deriving stock (Show, Eq)

data Opcode = Add (Mode, Mode)
            | Mul (Mode, Mode)
            | In
            | Out Mode
            | JumpTrue (Mode, Mode)
            | JumpFalse (Mode, Mode)
            | LessThan (Mode, Mode)
            | Equals (Mode, Mode)
            | AdjustBase Mode
            | Halt
  deriving stock Show

data Mode = Position | Immediate
  deriving stock Show

data ComputerState = ComputerState {
  input  :: [Int],
  status :: Status,
  pp     :: Int,
  memory :: Array Int Int,
  base :: Int
  } deriving stock (Generic, Show)

initial_state :: [Int] -> ComputerState
initial_state code = ComputerState {
  input = [ ],
  status = Running,
  pp = 0,
  memory = listArray (0, length code - 1) code,
  base = 0
  }

