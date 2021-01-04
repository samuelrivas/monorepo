{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLabels      #-}

module Advent.Day23.Internal (
  Game (Game),
  mkGame
  ) where

import           Advent.Perlude
import qualified Prelude              (Show, show)

import           Control.Lens         (at, view)
import           Data.Foldable        (fold)
import           Data.Generics.Labels ()
import           Data.HashMap.Strict  (HashMap, fromList, size)
import           Data.Maybe           (fromJust)
import           GHC.Generics         (Generic)

-- TODO: Check the Deque.Strict.Reader and Deque.Strict.State modules, they
-- probably simplify this

data Game = Game
    { pos  :: Int
    , cups :: HashMap Int Int
    }
    deriving stock (Generic, Eq)

instance Prelude.Show Game where
  show g =
    let
      n = size . cups $ g
      readElement i =
        let element = Prelude.show . fromJust . view (#cups . at i) $ g
        in if i == view #pos g then "(" ++ element ++ ")"
           else element
    in
      fold $ readElement <$> [0 .. (n - 1)]


mkGame :: [Int] -> Game
mkGame = Game 0 . fromList . zip [0..]
