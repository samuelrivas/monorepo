{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}


module Advent.Day12.Internal (
  Ship (Ship),
  Ship2 (Ship2),
  Direction (..),
  Action (..),
  Instruction (Instruction),
  mkShip,
  mkShip2,
  mkInstruction
  ) where

import           Advent.Perlude

import           Data.Bidim     (Coord)
import           GHC.Generics   (Generic)

data Direction = N
    | E
    | S
    | W
    deriving stock (Show, Eq, Enum)

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

data Ship2 = Ship2
    { position  :: Coord
    , viewpoint :: Coord
    }
    deriving stock (Show, Eq, Generic)

data Instruction = Instruction
    { action :: Action
    , amount :: Int
    }
    deriving stock (Show, Eq, Generic)

mkShip :: Ship
mkShip = Ship (0, 0) E

mkShip2 :: Ship2
mkShip2 = Ship2 (0, 0) (10, 1)

mkInstruction :: Action -> Int -> Instruction
mkInstruction = Instruction
