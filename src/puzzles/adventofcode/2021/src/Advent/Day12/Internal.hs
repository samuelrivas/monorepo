{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Advent.Day12.Internal (
  Path (Path)
  ) where

import           Perlude

import           Data.HashSet       (HashSet)
import           Data.List.NonEmpty (NonEmpty)
import           GHC.Generics       (Generic)

-- Paths are reversed, last visited is in head
data Path = Path
    { route      :: NonEmpty Text,
      smallCaves :: HashSet Text,
      smallCave  :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)
