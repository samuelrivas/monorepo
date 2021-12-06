{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Advent.Day6.Internal (
  SchoolState (..)
  ) where

import           Perlude

import           Data.Map.Strict (Map, fromList)
import           GHC.Generics    (Generic)

data SchoolState = SchoolState
    { day    :: Int
    , school :: Map Int Int
    }
    deriving stock (Show, Eq, Generic)

