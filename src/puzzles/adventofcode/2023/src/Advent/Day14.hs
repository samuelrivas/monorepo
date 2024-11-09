{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day14 where

import           Perlude

import           Data.Advent          (Day (..))
import           Data.Maybe           (fromJust)
import           Data.Text            (intercalate)
import           System.IO.Advent     (getInput, getParsedInput)
import           Text.Parsec          (anyChar, skipMany)
import           Text.Parsec.Parselib (Parser, unsafeParseAll)
type Parsed = ()

day :: Day
day = D14

rawInput :: IO Text
rawInput = getInput day

example :: Text
example = intercalate "\n" [
  ]

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parsedInput :: IO Parsed
parsedInput = getParsedInput day parser

parser :: Parser Parsed
parser = skipMany anyChar

solve1 :: Parsed -> ()
solve1 = const ()

solve2 :: Parsed -> ()
solve2 = const ()

main :: IO ()
main = do
  input <- getParsedInput day parser

  putStr "Solution 1: "
  print $ solve1 input

  putStr "Solution 2: "
  print $ solve2 input
