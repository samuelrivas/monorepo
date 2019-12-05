{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
module State (
  initial_state,
  Status (..),
  ComputerState
  ) where

import           Data.Array           (Array)
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
