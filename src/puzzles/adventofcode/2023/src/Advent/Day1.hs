{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day1 where

import           Perlude

import           Data.Advent          (Day (..))
import           Data.Char            (digitToInt, isDigit)
import           Data.Maybe           (fromJust)
import           Data.Num.Advent      (numListToDec)
import           Data.Text            as Text
import           System.IO.Advent     (getInput, getParsedInput)
import           Text.Parsec.Char     (noneOf)
import           Text.Parsec.Parselib (Parser, linesOf, text, unsafeParseAll)

type Parsed = [Text]

day :: Day
day = D1

rawInput :: IO Text
rawInput = getInput day

example :: Text
example = intercalate "\n" [
  "1abc2",
  "pqr3stu8vwx",
  "a1b2c3d4e5f",
  "treb7uchet"
  ]

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parsedInput :: IO Parsed
parsedInput = getParsedInput day parser

parser :: Parser Parsed
parser = linesOf $ text (noneOf ['\n'])

calibrationValue :: Text -> Int
calibrationValue txt =
  numListToDec. fromJust $ do
  low <- find isDigit txt
  high <- find isDigit (Text.reverse txt)
  return $ digitToInt <$> [low, high]

solve1 :: Parsed -> Int
solve1 = sum . fmap calibrationValue

solve2 :: Parsed -> ()
solve2 = const ()

main :: IO ()
main = do
  input <- getParsedInput day parser

  putStr "Solution 1: "
  print $ solve1 input

  putStr "Solution 2: "
  print $ solve2 input
