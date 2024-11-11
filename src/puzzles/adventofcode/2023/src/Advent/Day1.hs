{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day1 where

import           Perlude

import           Control.Applicative  ((<|>))
import           Data.Advent          (Day (..))
import           Data.Char            (digitToInt, isDigit)
import           Data.Functor         (($>))
import           Data.Maybe           (catMaybes, fromJust)
import           Data.Num.Advent      (numListToDec)
import           Data.Text            as Text
import           System.IO.Advent     (getInput, getParsedInput)
import           Text.Parsec          (many1, oneOf, sepEndBy, try)
import           Text.Parsec.Char     (noneOf)
import           Text.Parsec.Parselib (Parser, linesOf, literal, text,
                                       unsafeParseAll)
data Digit = Literal Int | Textual Int
  deriving Show

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
parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parsedInput :: IO Parsed
parsedInput = getParsedInput day parser

-- parser :: Parser Parsed
-- parser = linesOf $ text (noneOf ['\n'])

parser :: Parser [[Digit]]
parser = linesOf $ catMaybes <$> many1 maybeDigit

whitespace :: Parser ()
whitespace = noneOf ['\n'] $> ()

maybeDigit :: Parser (Maybe Digit)
maybeDigit =
  Just <$> digit
  <|> noneOf  ['\n'] $> Nothing

digit :: Parser Digit
digit =
  try (literal "one") $> Textual 1
  <|> try (literal "two") $> Textual 2
  <|> try (literal "three") $> Textual 3
  <|> try (literal "four") $> Textual 4
  <|> try (literal "five") $> Textual 5
  <|> try (literal "six") $> Textual 6
  <|> try (literal "seven") $> Textual 7
  <|> try (literal "eight") $> Textual 8
  <|> try (literal "nine") $> Textual 9
  <|> Literal . digitToInt <$> try (oneOf ['0'..'9'])

firstLiteral :: [Digit] -> Int
firstLiteral ((Literal x) : _) = x
firstLiteral (_ : t)           = firstLiteral t
firstLiteral _                 = error "can't find digit"

toInt :: Digit -> Int
toInt (Literal x) = x
toInt (Textual x) = x

calibrationValue :: [Digit] -> Int
calibrationValue digits =
  10 * (firstLiteral digits) + firstLiteral (Perlude.reverse digits)

calibrationValue2 :: [Digit] -> Int
calibrationValue2 digits =
  10 * (toInt $ Perlude.head digits) + (toInt . Perlude.last $ digits)


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
