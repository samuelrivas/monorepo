{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module Advent.Day21 where

import           Perlude

import           Advent.Templib       (linesOf)

import           Control.Applicative  ((<|>))
import           Control.Lens         (Each (each), Prism', _2, _Just, preview,
                                       prism, sumOf)
import           Data.Advent          (Day (..))
import           Data.Generics.Labels ()
import           Data.List            (foldl1')
import           Data.Maybe           (fromJust)
import           Data.Text            (intercalate)
import qualified Prelude
import           System.IO.Advent     (getInput, solve)
import           Text.Parsec          (char)
import           Text.Parsec.Parselib (Parser, digitAsNum, unsafeParseAll)

type Parsed = [Int]

day :: Day
day = D21

rawInput :: IO Text
rawInput = getInput day

example :: Text
example =
  intercalate
  "\n" [
  "Player 1 starting position: 4",
  "Player 2 starting position: 8"
  ]

expensiveDie :: [Int]
expensiveDie = (+ 1) . (`mod` 100) <$> [0..]

expensiveThrow :: [Int]
expensiveThrow =
  let
    throw3 (a:b:c:t) = (a + b + c) : throw3 t
    throw3 _         = error "this should not happen"
  in
    throw3 expensiveDie

-- This returns the result of summing 3 dice rolls after rolling 3 dice n times
-- before. Modulo 10 because the board has only 10 spaces. If we don't do modulo
-- we need to deal with the complexity of the die warping at 100
threeDice :: Int -> Int
threeDice n = (6 + 9 * n) `mod` 10

takeDrop :: [a] -> [a]
takeDrop (x : _ : t) = x : takeDrop t
takeDrop [x]         = [x]
takeDrop _           = []

player1Rolls :: [Int]
player1Rolls = threeDice <$> takeDrop [0..]

player2Rolls :: [Int]
player2Rolls = threeDice <$> takeDrop [1..]

-- Players move in cycles, the one for first player is shorter than the one for
-- player 2
spaces :: Int -> [Int] -> [Int]
spaces start rolls =
  fmap (`mod` 10) . zipWith (+) (start : spaces start (tail rolls)) $ rolls

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parser :: Parser Parsed
parser = undefined

solver1 :: Parsed -> Int
solver1 = undefined

solver2 :: Parsed -> Int
solver2 = undefined

main :: IO ()
main = solve day parser solver1 solver2
