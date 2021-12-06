{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day6 where

import           Perlude

import           Advent.Day6.Internal

import           Control.Lens         (assign, at, modifying, non, sumOf, use)
import           Control.Monad        (replicateM_)
import           Control.Monad.State  (MonadState, evalState, gets)
import           Data.Advent          (Day (..))
import           Data.Generics.Labels ()
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HashMap
import           Data.Maybe           (fromJust)
import           System.IO.Advent     (getInput, solve)
import           Text.Parsec          (char, sepBy)
import           Text.Parsec.Parselib (Parser, digitsAsNum, unsafeParseAll)

type Parsed = [Int]

day :: Day
day = D6

rawInput :: IO Text
rawInput = getInput day

example :: Text
example = "3,4,3,1,2"

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parser :: Parser Parsed
parser = digitsAsNum `sepBy` char ','

-- TODO do this with arrays.
-- Maps any day in the fish cycle is to the amount of fish on that day
toSchool :: [Int] -> HashMap Int Int
toSchool = HashMap.fromListWith (+) . flip zip (repeat 1)

mkState :: [Int] -> SchoolState
mkState = SchoolState 0 0 0 0 . toSchool

step :: MonadState SchoolState m => m ()
step =
  do
    today <- use #day
    eighters <- use #eigthers
    seventhers <- use #seventhers
    sixthers <- use #sixthers
    newFish <- use (#school . at (today `mod` 7) . non 0)

    assign #eigthers newFish
    assign #seventhers eighters
    assign #sixthers seventhers
    modifying (#school . at ((today - 1) `mod` 7) . non 0) (+ sixthers)
    modifying #day (+1)

totalFish :: MonadState SchoolState m => m Int
totalFish = do
  inschool <- gets $ sumOf (#school . traverse)
  eigthers <- use #eigthers
  seventhers <- use #seventhers
  sixthers <- use #sixthers
  pure $ sum [inschool, eigthers, seventhers, sixthers]

advanceDays :: MonadState SchoolState m => Int -> m ()
advanceDays n = replicateM_ n step

solver1 :: Parsed -> Int
solver1 = evalState (advanceDays 80 >> totalFish) . mkState

solver2 :: Parsed -> Int
solver2 = evalState (advanceDays 256 >> totalFish) . mkState

main :: IO ()
main = solve day parser solver1 solver2
