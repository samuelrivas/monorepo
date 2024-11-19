{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Re-solve stretch: Use only algebraic datatypes, do not rely on lists of
-- colours

module Advent.Day2 where

import           Perlude

import           Control.Lens         (allOf)
import           Data.Advent          (Day (..))
import           Data.Foldable        (foldl')
import           Data.Functor         (($>))
import           Data.Maybe           (fromJust)
import           Data.Text            (intercalate)
import           System.IO.Advent     (getInput, getParsedInput)
import           Text.Parsec          (choice, sepBy)
import           Text.Parsec.Parselib (Parser, digitsAsNum, linesOf, literal,
                                       unsafeParseAll)

data Colour = Red | Green | Blue
  deriving stock (Show, Eq)

data Game = Game Int [[(Int, Colour)]]
  deriving stock Show

-- TODO: lens this
gameNumber :: Game -> Int
gameNumber (Game n _) = n

gameRounds :: Game -> [[(Int, Colour)]]
gameRounds (Game _ rs) = rs

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

underMax :: (Int, Colour) -> Bool
underMax (x, Red)   = x <= 12
underMax (x, Green) = x <= 13
underMax (x, Blue)  = x <= 14

validGame :: Game -> Bool
validGame (Game _ rounds) = allOf (traverse . traverse) underMax rounds

-- TODO deuglify this
maxes :: [(Int, Colour)] -> (Int, Int, Int)
maxes l =
  let
    f (r, g, b) = \case
      (r', Red) -> (max r' r, g, b)
      (g', Green) -> (r, max g' g, b)
      (b', Blue) -> (r, g, max b' b)
  in
    foldl' f (0, 0, 0) l

power :: (Int, Int, Int) -> Int
power (r, g, b) = r * g * b

solve1 :: Parsed -> Int
solve1 = sum . fmap gameNumber . filter validGame

solve2 :: Parsed -> Int
solve2 x =
  let
    allRounds = concat . gameRounds <$> x
  in
    sum $ power . maxes <$> allRounds

main :: IO ()
main = do
  input <- getParsedInput day parser

  putStr "Solution 1: "
  print $ solve1 input

  putStr "Solution 2: "
  print $ solve2 input
