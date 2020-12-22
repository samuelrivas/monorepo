{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Advent.Day22.Internal (
  Game (Game),
  mkGame
  ) where

import           Advent.Perlude
import qualified Prelude        (Show, show)

import           Data.Foldable  (fold)
import           GHC.Generics   (Generic)

data Game = Game
    { deck1 :: [Int]
    , deck2 :: [Int]
    }
    deriving stock (Generic, Eq)

mkGame :: [Int] -> [Int] -> Game
mkGame = Game

instance Prelude.Show Game where
  show g =
    fold [
    "\nPlayer 1: ",
    Prelude.show (deck1 g),
    "\nPlayer 2:",
    Prelude.show (deck2 g),
    "\n"]
