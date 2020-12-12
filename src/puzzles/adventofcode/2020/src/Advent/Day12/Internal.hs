{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Advent.Day12.Internal (
  Ship (Ship),
  Direction (..),
  Action (..),
  Instruction (Instruction),
  mkShip,
  mkInstruction
  ) where

import           Advent.Perlude

import           Data.Bidim     (Coord)
import           GHC.Generics   (Generic)

data Direction = N
    | S
    | W
    | E
    deriving stock (Show, Eq)

 -- True is clockwise
data Action = F
    | T Bool
    | M Direction
    deriving stock (Show, Eq)

data Ship = Ship
    { position  :: Coord
    , direction :: Direction
    }
    deriving stock (Show, Eq, Generic)

data Instruction = Instruction
    { action :: Action
    , amount :: Int
    }
    deriving stock (Show, Eq, Generic)

mkShip :: Ship
mkShip = Ship (0, 0) E

mkInstruction :: Action -> Int -> Instruction
mkInstruction = Instruction
