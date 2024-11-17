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
import           Data.Maybe                     (catMaybes, fromJust)
import           Data.Text                      as Text
import           GHC.Generics                   (Generic)
import           System.IO.Advent               (getInput, getParsedInput)
import           Text.Parsec                    (anyChar, lookAhead, many1,
                                                 oneOf, try)
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
parser = linesOf $ catMaybes <$> many1 maybeDigit

whitespace :: Parser ()
whitespace = noneOf ['\n'] $> ()

maybeDigit :: Parser (Maybe Digit)
maybeDigit =
  Just <$> digit
  <|> whitespace $> Nothing

consumeOne :: Text -> Parser Text
consumeOne t = (lookAhead . literal $ t) <* anyChar

skipUntil :: Parser a -> Parser b -> Parser b
skipUntil a b =
  b <|> a *> skipUntil a b

digit :: Parser Digit
digit =
  try (consumeOne "one") $> Textual 1
  <|> try (consumeOne "two") $> Textual 2
  <|> try (consumeOne "three") $> Textual 3
  <|> try (consumeOne "four") $> Textual 4
  <|> try (consumeOne "five") $> Textual 5
  <|> try (consumeOne "six") $> Textual 6
  <|> try (consumeOne "seven") $> Textual 7
  <|> try (consumeOne "eight") $> Textual 8
  <|> try (consumeOne "nine") $> Textual 9
  <|> Literal . digitToInt <$> oneOf ['0'..'9']

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
