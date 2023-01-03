{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Advent.Day15.Internal (
  GameState (GameState),
  mkGameState
  ) where

import           Perlude

import           Data.Map     (Map, fromList)
import           GHC.Generics (Generic)

data GameState = GameState
    { ages       :: Map Int Int
    , lastSpoken :: Int
    , clock      :: Int
    }
    deriving stock (Generic, Eq, Show)

mkGameState :: [Int] -> GameState
mkGameState initial = GameState {
  ages = fromList $ zip (init initial) [0..],
  lastSpoken = last initial,
  clock = length initial - 1
  }
