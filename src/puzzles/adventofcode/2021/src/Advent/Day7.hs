{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day7 where

import           Perlude

import           Data.Advent          (Day (..))
import           Data.Generics.Labels ()
import           Data.Maybe           (fromJust)
import           System.IO.Advent     (getInput, solve)
import           Text.Parsec          (char, sepBy)
import           Text.Parsec.Parselib (Parser, digitsAsNum, unsafeParseAll)

type Parsed = [Int]

day :: Day
day = D7

rawInput :: IO Text
rawInput = getInput day

example :: Text
example = "16,1,2,0,4,2,7,1,2,14"

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parser :: Parser Parsed
parser = digitsAsNum `sepBy` char ','

-- TODO this should go to library
distance :: Num n => n -> n -> n
distance a b = abs (a - b)

distance2 :: Integral n => n -> n -> n
distance2 a b =
  let n = distance a b
  in (n + 1) * n `div` 2

fuelNeeded :: (Int -> Int -> Int) -> Int -> [Int] -> Int
fuelNeeded distanceFunction dest  = sum . fmap (distanceFunction dest)

-- TODO This is solvable with bisection (there may be even faster analytical
-- solutions). Get a bisection function into adventlib! (maybe is even an
-- instance of MonadSearch)

solver :: (Int -> Int -> Int) -> [Int] -> Int
solver distanceFunction input =
  minimum
  $ flip (fuelNeeded distanceFunction) input
  <$> [(minimum input)..(maximum input)]

solver1 :: Parsed -> Int
solver1 = solver distance

solver2 :: Parsed -> Int
solver2 = solver distance2

main :: IO ()
main = solve day parser solver1 solver2
