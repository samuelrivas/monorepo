{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day5 where

import           Perlude

import           Advent.Templib       (linesOf)

import           Control.Lens         (_2, at, filtered, lengthOf, non, over,
                                       view)
import           Control.Monad.State  (MonadState, evalState, gets, modify)
import           Data.Advent          (Day (..))
import           Data.Bidim           (Coord, showBidim)
import           Data.Foldable        (traverse_)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Map             as Map
import           Data.Maybe           (fromJust)
import           Data.MultiSet        (MultiSet)
import qualified Data.MultiSet        as MultiSet
import           Data.Ratio           (denominator, numerator, (%))
import           Data.Text            (intercalate)
import           System.IO.Advent     (getInput, solve)
import           Text.Parsec          (char, sepBy)
import           Text.Parsec.Parselib (Parser, digitsAsNum, literal,
                                       unsafeParseAll)

type Parsed = [Int]

day :: Day
day = D5

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
toState :: [Int] -> HashMap Int Int
toState = HashMap.fromListWith (+) . flip zip (repeat 1)

nextDay :: Int -> HashMap Int Int -> HashMap Int Int
NextDay tick fish =
  let
    newFish = view (at tick . non 0) fish
    newSeventhers = view (at 8 . non 0) fish
    newSixthers = view (at 7 . non 0) fish
  in
    over (at 8 . non 0) (+ newFish)
    . over (at 7 . non 0) (+ newSeventhers)
    . over (at ? . non 0) (+ newSixthers)
    $ fish

solver1 :: Parsed -> Int
solver1 = undefined

solver2 :: Parsed -> Int
solver2 = undefined

main :: IO ()
main = solve day parser solver1 solver2
