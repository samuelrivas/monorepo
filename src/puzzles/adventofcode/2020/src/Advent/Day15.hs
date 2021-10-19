{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day15 where

import           Perlude

import           Control.Lens               (assign, at, modifying, use, view)
import           Control.Monad              (replicateM_)
import           Control.Monad.State.Strict (MonadState, evalState)
import           Data.Generics.Labels       ()
import qualified Data.Map                   as Map
import           Text.Parsec                (char, sepBy)


import           Advent.Day15.Internal
import           Advent.Templib             (Day (..), getInput',
                                             getParsedInput)
import           Advent.Templib.Parsec      (Parser, digitsAsNum)

day :: Day
day = D15

getInput :: IO Text
getInput = getInput' D15

parser :: Parser [Int]
parser = digitsAsNum `sepBy` char ','

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
  input <- getParsedInput day parser

  putStr "Solution 1: "
  print $ solve input  2020

  -- TODO: Optimise

  -- This may be also a good case for some extra visibility monad, like
  -- MonadSample or something that allows us to run long computations but see
  -- what is going on
  putStr "Solution 2: "
  print $ solve input 30000000
