{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Advent.Day6.Internal (
  SchoolState (SchoolState)
  ) where

import           Perlude

import           Data.HashMap.Strict (HashMap)
import           GHC.Generics        (Generic)

data SchoolState = SchoolState
    { day        :: Int,
      eigthers   :: Int,
      seventhers :: Int,
      sixthers   :: Int,
      school     :: HashMap Int Int
    }
    deriving stock (Show, Eq, Generic)

