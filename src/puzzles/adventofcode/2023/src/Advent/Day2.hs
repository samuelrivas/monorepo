{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day2 where

import           Perlude

import           Control.Lens          (Lens', filtered, set, sumOf, traversed,
                                        view)
import           Data.Advent           (Day (..))
import           Data.Coerce           (coerce)
import           Data.Foldable         (fold)
import           Data.Functor          (($>))
import           Data.Generics.Product (position)
import           Data.Maybe            (fromJust)
import           Data.Semigroup        (Max (..), Sum (..))
import           Data.Text             (intercalate)
import           Fmt                   (fmt, (+|), (|+))
import           GHC.Internal.Generics (Generic)
import           System.IO.Advent      (getInput, getParsedInput)
import           Text.Parsec           (choice, sepBy)
import           Text.Parsec.Parselib  (Parser, digitsAsNum, linesOf, literal,
                                        unsafeParseAll)
import           Text.Show             (show)

data Round a = Round a a a
  deriving stock (Generic)

data Game a = Game Int [Round a]
  deriving stock (Show, Generic)

instance Semigroup a => Semigroup (Round a) where
  (Round r g b) <> (Round r' g' b') = Round (r <> r') (g <> g') (b <> b')

instance Monoid a => Monoid (Round a) where
  mempty = Round mempty mempty mempty

instance Show a => Show (Round a) where
  show (Round r g b) = fmt $
    "(" +|
    Perlude.show r |+ "," +|
    Perlude.show g |+ "," +|
    Perlude.show b |+ ")"

gameNumber :: Lens' (Game a) Int
gameNumber = position @1

gameRounds :: Lens' (Game a) [Round a]
gameRounds = position @2

red :: Lens' (Round a) a
red = position @1

green :: Lens' (Round a) a
green = position @2

blue :: Lens' (Round a) a
blue = position @3

type Parsed = [Game (Sum Int)]

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

parsedExample :: (Monoid a, Num a) => [Game a]
parsedExample = fromJust $ unsafeParseAll parser example

parsedInput :: (Monoid a, Num a) => IO [Game a]
parsedInput = getParsedInput day parser

parser :: (Monoid a, Num a) => Parser [Game a]
parser = linesOf gameP

gameP :: (Monoid a, Num a) => Parser (Game a)
gameP =
  Game
  <$> gameNumberP <* literal ": "
  <*> roundsP

roundsP :: (Monoid a, Num a) => Parser [Round a]
roundsP = roundP `sepBy` literal "; "

roundP :: (Monoid a, Num a) => Parser (Round a)
roundP = fold <$> ballCountP `sepBy` literal ", "

-- Parses a single ball count into a Round where all the other counts are mempty

ballCountP :: (Monoid a, Num a) => Parser (Round a)
ballCountP =
  do
    n <- digitsAsNum <* literal " "
    choice [
      literal "red" $> set red n,
      literal "green" $> set green n,
      literal "blue" $> set blue n
      ] <*> pure mempty

gameNumberP :: Parser Int
gameNumberP = literal "Game " *> digitsAsNum

validRound :: (Num a, Ord a) => Round a -> Bool
validRound (Round r g b) =
  r <= 12 &&
  g <= 13 &&
  b <= 14

validGame :: (Num a, Ord a) => Game a -> Bool
validGame (Game _ rounds) = all validRound rounds

power :: Num a => Round a -> a
power r  = view red r * view green r * view blue r

solve1 :: [Game (Sum Int)] -> Int
solve1 = sumOf (traversed . filtered validGame . gameNumber)

solve2 :: [Game (Max Int)] -> Int
solve2 x =
  let
    maxRounds = view gameRounds <$> x
    maxes :: [Round (Sum Int)] = coerce . fold <$> maxRounds
  in
    coerce . fold $ (power <$> maxes)

main :: IO ()
main = do
  input <- getParsedInput day parser

  putStr "Solution 1: "
  print $ solve1 input

  putStr "Solution 2: "
  print $ solve2 (coerce input)
