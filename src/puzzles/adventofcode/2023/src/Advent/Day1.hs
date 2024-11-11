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
import           Data.Maybe           (fromJust)
import           Data.Num.Advent      (numListToDec)
import           Data.Text            as Text
import           System.IO.Advent     (getInput, getParsedInput)
import           Text.Parsec          (many1, oneOf, try)
import           Text.Parsec.Char     (noneOf)
import           Text.Parsec.Parselib (Parser, linesOf, literal, text,
                                       unsafeParseAll)

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

parser :: Parser Parsed
parser = linesOf $ text (noneOf ['\n'])

parser2 :: Parser [[(Text, Maybe Int)]]
parser2 = linesOf $ many1 token

token :: Parser (Text, Maybe Int)
token =
  (, Just 1) <$> try (literal "one")
  <|> (, Just 2) <$> try (literal "two")
  <|> (, Just 3) <$> try (literal "three")
  <|> (, Just 4) <$> try (literal "four")
  <|> (, Just 5) <$> try (literal "five")
  <|> (, Just 6) <$> try (literal "six")
  <|> (, Just 7) <$> try (literal "seven")
  <|> (, Just 8) <$> try (literal "eight")
  <|> (oneOf ['0'..'1'] >>= \x -> return (Text.singleton x, Just $ digitToInt x))
  <|> (noneOf ['\n']  >>= \x -> return (Text.singleton x, Nothing))

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
