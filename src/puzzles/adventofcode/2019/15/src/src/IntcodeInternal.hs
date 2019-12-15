-- This module exists mainly to keep accessors from leaking out of records

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedLabels   #-}
module IntcodeInternal (
  Status (..),
  IntcodeState (IntcodeState),
  Opcode (..),
  Mode (..),
  initialState,
  show
  ) where

import           Prelude              hiding (show)
import qualified Prelude

import           Data.Generics.Labels ()
import           Data.Map.Strict      (Map, fromList)
import           Data.Text            (Text, pack)
import           GHC.Generics         (Generic)

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

data IntcodeState = IntcodeState {
  input  :: [Integer],
  output :: [Integer],
  status :: Status,
  pp     :: Integer,
  memory :: Map Integer Integer,
  base   :: Integer
  } deriving stock (Generic, Show)

initialState :: [Integer] -> IntcodeState
initialState code = IntcodeState {
  input = [ ],
  output = [ ],
  status = Running,
  pp = 0,
  memory = fromList (zip [0..] code),
  base = 0
  }
