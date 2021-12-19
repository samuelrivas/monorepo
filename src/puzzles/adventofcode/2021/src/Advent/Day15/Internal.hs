{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Advent.Day15.Internal (
  Node (Node)
  ) where

import           Perlude

import           Data.Bidim         (Coord)
import           Data.HashSet       (HashSet)
import           Data.Hashable      (Hashable)
import           Data.List.NonEmpty (NonEmpty)
import           GHC.Generics       (Generic)

data Node = Node
    { path    :: NonEmpty Coord,
      pathMem :: HashSet Coord,
      cost    :: Int
    }
    deriving stock (Show, Eq, Generic)

instance Hashable Node
