-- This module exists mainly to keep accessors from leaking out of records

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedLabels   #-}
module Advent.Day7.Internal (
  Status (..),
  ComputerState (ComputerState),
  Opcode (..),
  Mode (..),
  ) where

import           Data.Array           (Array)
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
            | Halt
  deriving stock Show

data Mode = Position | Immediate
  deriving stock Show

data ComputerState = ComputerState {
  input  :: [Int],
  status :: Status,
  pp     :: Int,
  memory :: Array Int Int
  } deriving stock (Generic, Show)
