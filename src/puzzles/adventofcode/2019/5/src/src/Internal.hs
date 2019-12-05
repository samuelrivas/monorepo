-- This module exists mainly to keep accessors from leaking out of records

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedLabels   #-}
module Internal (
  Status (..),
  ComputerState (ComputerState),
  Instruction (Instruction),
  Opcode (..),
  Mode (..),
  ) where

import           Data.Array           (Array, listArray)
import           Data.Generics.Labels ()
import           GHC.Generics         (Generic)

{-# ANN module "HLint: ignore Use camelCase" #-}

data Status = Running | Finished | Aborted
  deriving stock (Show, Eq)

data Opcode = Add | Mul | Halt
  deriving stock Show

data Mode = Position | Value
  deriving stock Show

data ComputerState = ComputerState {
  status :: Status,
  pp     :: Int,
  memory :: Array Int Int
  } deriving stock (Generic, Show)

data Instruction = Instruction {
  opcode :: Opcode,
  modes  :: [Mode]
  } deriving stock (Generic, Show)

