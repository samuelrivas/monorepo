{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Advent.Day8.Internal (
  Op (..),
  VMState (VMState),
  mkState
  ) where

import           Advent.Perlude

import           Data.Map.Strict (Map, fromList)
import           GHC.Generics    (Generic)

data Op = Nop Int
    | Acc Int
    | Jmp Int
    deriving stock (Show, Eq)

data VMState = VMState
    { pc   :: Int
    , code :: Map Int Op
    , acc  :: Int
    }
    deriving stock (Show, Eq, Generic)

mkState :: [Op] -> VMState
mkState ops =
  VMState {
    pc = 1,
    code = fromList . zip [1..] $ ops,
    acc = 0
  }
