{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Advent.Day4.Internal (
  Coord,
  Boards (Boards),
  BingoState,
  ) where

import           Perlude

import           Data.HashMap.Strict (HashMap)
import           Data.HashSet        (HashSet)
import           GHC.Generics        (Generic)

-- Row, Column, Board
type Coord = (Int, Int, Int)

-- TODO Create a HashMultiMap utility, or find one in libraries
data Boards = Boards
    { direct  :: HashMap Coord Int,
      inverse :: HashMap Int (HashSet Coord)
    }
    deriving stock (Show, Eq, Generic)

type BingoState = HashSet Coord
