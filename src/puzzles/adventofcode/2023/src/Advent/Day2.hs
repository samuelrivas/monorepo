{-# LANGUAGE DataKinds           #-}
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

import           Control.Lens          (Lens', allOf, each, filtered, productOf,
                                        set, sumOf, traversed, view)
import           Data.Advent           (Day (..))
import           Data.Coerce           (coerce)
import           Data.Foldable         (fold)
import           Data.Functor          (($>))
import           Data.Generics.Product (position)
import           Data.Maybe            (fromJust)
import           Data.Text             (intercalate)
import           Fmt                   (fmt, (+|), (|+))
import           GHC.Internal.Generics (Generic)
import           System.IO.Advent      (getInput, getParsedInput)
import           Text.Parsec           (choice, sepBy)
import           Text.Parsec.Parselib  (Parser, digitsAsNum, linesOf, literal,
                                        unsafeParseAll)
import           Text.Show             (show)

data Colour = Red | Green | Blue
  deriving stock (Show, Eq, Generic)

data Game = Game Int [[(Int, Colour)]]
  deriving stock (Show, Generic)

data Round = Round Red' Green' Blue'
  deriving stock (Generic)

-- TODO Try to implement this as a single parameterised type where the color is
-- a type literal
newtype Red' = Red' Int
  deriving stock (Generic)
  deriving newtype (Num, Ord, Eq)

newtype Green' = Green' Int
  deriving stock (Generic)
  deriving newtype (Num, Ord, Eq)

newtype Blue' = Blue' Int
  deriving stock (Generic)
  deriving newtype (Num, Ord, Eq)

data Game' = Game' Int [Round]
  deriving stock (Show, Generic)

instance Semigroup Round where
  (Round r g b) <> (Round r' g' b') = Round (r + r') (g + g') (b + b')

instance Monoid Round where
  mempty = Round 0 0 0

instance Show Round where
  show (Round r g b) = fmt $
    "(" +|
    Perlude.show r |+ "," +|
    Perlude.show g |+ "," +|
    Perlude.show b |+ ")"

instance Show Red' where
  show (Red' n) = "r" +| n |+ ""

instance Show Green' where
  show (Green' n) = "g" +| n |+ ""

instance Show Blue' where
  show (Blue' n) = "b" +| n |+ ""

gameNumber :: Lens' Game Int
gameNumber = position @1

gameNumber' :: Lens' Game' Int
gameNumber' = position @1

gameRounds :: Lens' Game [[(Int, Colour)]]
gameRounds = position @2

red :: Lens' Round Red'
red = position @1

green :: Lens' Round Green'
green = position @2

blue :: Lens' Round Blue'
blue = position @3

type Parsed = [Game']

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

parser' :: Parser Parsed
parser' = linesOf gameP'

parser :: Parser [Game']
parser = linesOf gameP'

gameP' :: Parser Game'
gameP' =
  Game'
  <$> gameNumberP <* literal ": "
  <*> roundsP

roundsP :: Parser [Round]
roundsP = roundP `sepBy` literal "; "

roundP :: Parser Round
roundP = fold <$> ballCountP' `sepBy` literal ", "

-- Parses a single ball count into a Round where all the other counts are mempty
--
-- TODO: Improve this using a setter over a mempty Round
ballCountP' :: Parser Round
ballCountP' =
  do
    n :: Int <- digitsAsNum <* literal " "
    choice [
      literal "red" $> set red (coerce n),
      literal "green" $> set green (coerce n),
      literal "blue" $> set blue (coerce n)
      ] <*> mempty

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

validRound :: Round -> Bool
validRound (Round r g b) =
  r <= 12 &&
  g <= 13 &&
  b <= 14

validGame :: Game' -> Bool
validGame (Game' _ rounds) = all validRound rounds

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
power = productOf each

solve1 :: Parsed -> Int
solve1 = sumOf (traversed . filtered validGame . gameNumber')

solve2 :: Parsed -> Int
solve2 x = undefined
  -- let
  --   allRounds = concat . view gameRounds <$> x
  -- in
  --   sum $ power . maxes <$> allRounds

main :: IO ()
main = do
  input <- getParsedInput day parser

  putStr "Solution 1: "
  print $ solve1 input

  putStr "Solution 2: "
  print $ solve2 input
