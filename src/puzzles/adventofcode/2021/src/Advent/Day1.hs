{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day1 where

import           Perlude

import           Control.Lens         (each, sumOf)
import           Data.Advent          (Day (..))
import           System.IO.Advent     (getInput, solve)
import           Text.Parsec.Advent   (listOfNum)
import           Text.Parsec.Parselib (Parser)

day :: Day
day = D1

rawInput :: IO Text
rawInput = getInput day

parser :: Parser [Int]
parser = listOfNum

solver1 :: [Int] -> Int
solver1 l = length . filter (uncurry (<))  . zip l $ tail l

solver2 :: [Int] -> Int
solver2 l =
  let triplets = zip3 l (tail l) (tail . tail $ l)
  in solver1 $ sumOf each <$> triplets

main :: IO ()
main = solve day parser solver1 solver2

