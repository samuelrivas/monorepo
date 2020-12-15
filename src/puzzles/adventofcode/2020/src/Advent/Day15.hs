{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day15 where

import           Advent.Perlude

import           Control.Lens               (assign, at, modifying, use, view)
import           Control.Monad              (replicateM_)
import           Control.Monad.State.Strict (MonadState, evalState)
import           Data.Generics.Labels       ()
import qualified Data.Map                   as Map
import qualified Data.Text                  as Text
import qualified System.IO.Advent           as IOAdvent

import           Advent.Day15.Internal

getInput :: IO Text
getInput = IOAdvent.getInput "15"

parse :: Text -> [Int]
parse = fmap read . Text.splitOn ","

step :: MonadState GameState m => m Int
step = do
  lastSpoken <- use #lastSpoken
  clock <- use #clock
  ages <- use #ages

  let toSpeak = case view (at lastSpoken) ages of
        Just age -> clock - age
        Nothing  -> 0

  modifying #ages (Map.insert lastSpoken clock)
  modifying #clock (+1)
  assign #lastSpoken toSpeak

  pure toSpeak

solve :: [Int] -> Int -> Int
solve initial steps =
  flip evalState (mkGameState initial) $ do
    replicateM_ (steps - length initial) step
    use #lastSpoken

main :: IO ()
main = do
  input <- parse <$> getInput

  putStr "Solution 1: "
  print $ solve input  2020

  putStr "Solution 2: "
  print $ solve input 30000000
