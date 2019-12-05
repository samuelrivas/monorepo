{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedLabels   #-}
module State (
  initial_state,
  Status (..),
  ComputerState,
  read_immediate,
  read_position
  ) where

import           Control.Lens         (use)
import           Control.Monad.State  (MonadState)
import           Data.Array           (Array, (!))
import           Data.Array.IArray    (listArray)
import           Data.Generics.Labels ()
import           GHC.Generics         (Generic)

{-# ANN module "HLint: ignore Use camelCase" #-}

data Status = Running | Finished | Aborted
  deriving stock (Show, Eq)

data ComputerState = ComputerState {
  status :: Status,
  pp     :: Int,
  memory :: Array Int Int
  } deriving stock (Generic, Show)

initial_state :: [Int] -> ComputerState
initial_state list = ComputerState Running 0
  $ listArray (0, length list - 1) list

read_immediate :: MonadState ComputerState m => Int -> m Int
read_immediate pos = (! pos) <$> use #memory

read_position :: MonadState ComputerState m => Int -> m Int
read_position pos = read_immediate pos >>= read_immediate
