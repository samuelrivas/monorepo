-- This module exists mainly to keep accessors from leaking out of records

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE OverloadedStrings  #-}
module Control.Monad.Intcode.Internal (
  Status (..),
  IntcodeState (IntcodeState),
  Opcode (..),
  Mode (..),
  initialState
  ) where

import           Prelude

import           Data.Generics.Labels ()
import           Data.HashMap.Strict  (HashMap, fromList)
import qualified Data.List            as List
import           GHC.Generics         (Generic)

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

data IntcodeState = IntcodeState {
  input  :: [Integer],
  output :: [Integer],
  status :: Status,
  pp     :: Integer,
  memory :: HashMap Integer Integer,
  base   :: Integer
  } deriving stock (Generic)

instance Show IntcodeState where
  show st = List.intercalate " || " [
    "input: " <> show (input st),
    "output: " <> show (output st),
    "status: " <> show (status st),
    "pp: " <> show (pp st)
    ]

initialState :: [Integer] -> IntcodeState
initialState code = IntcodeState {
  input = [ ],
  output = [ ],
  status = Running,
  pp = 0,
  memory = fromList (zip [0..] code),
  base = 0
  }
