{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day2 where

import           Perlude

import           Data.Advent          (Day (..))
import           Data.Functor         (($>))
import           Data.Maybe           (fromJust)
import           Data.Text            (intercalate)
import           System.IO.Advent     (getInput, getParsedInput)
import           Text.Parsec          (anyChar, choice, sepBy, skipMany)
import           Text.Parsec.Parselib (Parser, digitAsNum, digitsAsNum, linesOf,
                                       literal, unsafeParseAll)

data Colour = Red | Green | Blue
  deriving stock Show

data Game = Game Int [[(Int, Colour)]]
  deriving stock Show

type Parsed = [Game]

day :: Day
day = D2

rawInput :: IO Text
rawInput = getInput day

example :: Text
example = intercalate "\n" [
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
  "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
  "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
  "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
  "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  ]

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parsedInput :: IO Parsed
parsedInput = getParsedInput day parser

parser :: Parser Parsed
parser = linesOf gameP

gameNumberP :: Parser Int
gameNumberP = literal "Game " *> digitsAsNum

colourP :: Parser Colour
colourP =
  choice [
  literal "red" $> Red,
  literal "green" $> Green,
  literal "blue" $> Blue
  ]

ballCountP :: Parser (Int, Colour)
ballCountP =
  (,)
  <$> digitsAsNum <* literal " "
  <*> colourP

subsetP :: Parser [(Int, Colour)]
subsetP = ballCountP `sepBy` literal ", "

subsetsP :: Parser [[(Int, Colour)]]
subsetsP = subsetP `sepBy` literal "; "

gameP :: Parser Game
gameP =
  Game
  <$> gameNumberP <* literal ": "
  <*> subsetsP

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
