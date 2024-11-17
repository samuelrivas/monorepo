{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day1 where

import           Perlude

import           Control.Applicative            ((<|>))
import           Control.Lens                   (firstOf)
import           Data.Advent                    (Day (..))
import           Data.Char                      (digitToInt)
import           Data.Functor                   (($>))
import           Data.Generics.Sum.Constructors (_Ctor)
import           Data.Maybe                     (fromJust)
import           Data.Text                      as Text
import           GHC.Generics                   (Generic)
import           System.IO.Advent               (getInput, getParsedInput)
import           Text.Parsec                    (anyChar, choice, digit,
                                                 lookAhead, many, notFollowedBy,
                                                 optional, sepEndBy, try)
import           Text.Parsec.Char               (noneOf)
import           Text.Parsec.Parselib           (Parser, linesOf, literal,
                                                 unsafeParseAll)

data Digit = Literal Int | Textual Int
  deriving stock (Show, Generic)

type Parsed = [[Digit]]

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

example2 :: Text
example2 = intercalate "\n" [
  "two1nine",
  "eightwothree",
  "abcone2threexyz",
  "xtwone3four",
  "4nineeightseven2",
  "zoneight234",
  "7pqrstsixteen"
  ]

-- This doesn't show on the examples, but textual numbers can overlap
exampleOverlap :: Text
exampleOverlap = "jjhxddmg5mqxqbgfivextlcpnvtwothreetwonerzk"

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parsedInput :: IO Parsed
parsedInput = getParsedInput day parser

parser :: Parser [[Digit]]
parser = linesOf line

line :: Parser [Digit]
line = optional whitespace *> digitToken `sepEndBy` whitespace

whitespace :: Parser ()
whitespace =
  let
    whitespaceChar = notFollowedBy digitToken <* noneOf ['\n']
  in
    many whitespaceChar $> ()

digitToken :: Parser Digit
digitToken = textualDigit <|> literalDigit

literalDigit :: Parser Digit
literalDigit =  Literal . digitToInt <$> digit

-- Digits can overlap, for example twone should be read as 2, 1. This parser
-- consumes only one character to achieve that
textualDigit :: Parser Digit
textualDigit =
  let
    digits = [
      ("one", 1),
      ("two", 2),
      ("three", 3),
      ("four", 4),
      ("five", 5),
      ("six", 6),
      ("seven", 7),
      ("eight", 8),
      ("nine", 9)
      ]
    toParser (txt, n) = (try . literal $ txt) $> Textual n
  in
    lookAhead (choice (toParser <$> digits)) <* anyChar

firstLiteral :: [Digit] -> Int
firstLiteral = fromJust . firstOf (traverse . _Ctor @"Literal")

toInt :: Digit -> Int
toInt (Literal x) = x
toInt (Textual x) = x

calibrationValue :: [Digit] -> Int
calibrationValue digits =
  10 * firstLiteral digits + firstLiteral (Perlude.reverse digits)

calibrationValue2 :: [Digit] -> Int
calibrationValue2 digits =
  10 * toInt (Perlude.head digits) + (toInt . Perlude.last $ digits)

solve1 :: Parsed -> Int
solve1 = sum . fmap calibrationValue

solve2 :: Parsed -> Int
solve2 = sum . fmap calibrationValue2

main :: IO ()
main = do
  input <- getParsedInput day parser

  putStr "Solution 1: "
  print $ solve1 input

  putStr "Solution 2: "
  print $ solve2 input
