{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day3 where

import           Perlude

import           Data.Advent          (Day (..))
import           Data.List            (transpose)
import           Data.Text            (intercalate)
import           System.IO.Advent     (getInput, solve)
import           Text.Parsec.Parselib (Parser)

import           Advent.Templib       (bitString, linesOf)
import           Data.Num.Advent      (bitListToDec)

type Parsed =  [[Bool]]

day :: Day
day = D3

rawInput :: IO Text
rawInput = getInput day

example :: Text
example = intercalate "\n" [
  "00100",
  "11110",
  "10110",
  "10111",
  "10101",
  "01111",
  "00111",
  "11100",
  "10000",
  "11001",
  "00010",
  "01010"]

parser :: Parser Parsed
parser = linesOf bitString

solver1 :: Parsed -> Int
solver1 l =
  let
    entries = length l
    gamma = (> entries `div` 2) <$> (length . filter id <$> transpose l)
    epsilon = not <$> gamma
  in
    bitListToDec gamma * bitListToDec epsilon

-- candidates and position to filter
filterOxygen :: [[Bool]] -> Int -> [[Bool]]
filterOxygen l pos =
  let
    bits = head . transpose $ (drop pos <$> l)
    ones = length (filter id bits)
    zeroes = length bits - ones
  in
    filter ((== (ones >= zeroes)). flip (!!) pos) l

-- candidates and position to filter
filterCo2 :: [[Bool]] -> Int -> [[Bool]]
filterCo2 l pos =
  let
    bits = head . transpose $ (drop pos <$> l)
    ones = length (filter id bits)
    zeroes = length bits - ones
  in
    filter ((== (ones < zeroes)). flip (!!) pos) l

getOxygen :: [[Bool]] -> Int -> [Bool]
getOxygen [oxygen] _ = oxygen
getOxygen l n        = getOxygen (filterOxygen l n) (n + 1)

getCo2 :: [[Bool]] -> Int -> [Bool]
getCo2 [co2] _ = co2
getCo2 l n     = getCo2 (filterCo2 l n) (n + 1)

solver2 :: Parsed -> Int
solver2 l =
  let
    oxygen = getOxygen l 0
    co2 = getCo2 l 0
  in
    bitListToDec oxygen * bitListToDec co2

main :: IO ()
main = solve day parser solver1 solver2
