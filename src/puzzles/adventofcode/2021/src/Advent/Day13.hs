{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-# LANGUAGE RankNTypes          #-}
module Advent.Day13 where

import           Perlude

import           Advent.Templib       (linesOf)

import           Control.Lens         (Lens', _1, _2, over, view)
import           Data.Advent          (Day (..))
import           Data.Bidim           (Coord, showBidim)
import           Data.Foldable        (foldl')
import           Data.Functor         (($>))
import           Data.Generics.Labels ()
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HashSet
import qualified Data.Map             as Map
import           Data.Maybe           (fromJust)
import           Data.Text            (intercalate)
import           System.IO.Advent     (getInput, getParsedInput)
import           Text.Parsec          (char, sepEndBy, (<|>))
import           Text.Parsec.Parselib (Parser, digitsAsNum, literal,
                                       unsafeParseAll)

data Instruction = FoldX Int | FoldY Int deriving (Eq, Show)

type Parsed =  ([Coord], [Instruction])

day :: Day
day = D13

rawInput :: IO Text
rawInput = getInput day

example :: Text
example =
  intercalate
  "\n" [
  "6,10",
  "0,14",
  "9,10",
  "0,3",
  "10,4",
  "4,11",
  "6,0",
  "6,12",
  "4,1",
  "0,13",
  "10,12",
  "3,4",
  "3,0",
  "8,4",
  "1,10",
  "2,14",
  "8,10",
  "9,0",
  "",
  "fold along y=7",
  "fold along x=5"
  ]

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parser :: Parser Parsed
parser =
  (,)
  <$> (coordP `sepEndBy` char '\n' <* literal "\n")
  <*> linesOf instructionP

coordP :: Parser Coord
coordP = (,) <$> (digitsAsNum <* char ',') <*> digitsAsNum

instructionP :: Parser Instruction
instructionP =
  do
    _ <- literal "fold along "
    constructor <- ((char 'x' $> FoldX) <|> (char 'y' $> FoldY)) <* char '='
    constructor <$> digitsAsNum

foldPaper :: Instruction -> HashSet Coord -> HashSet Coord
foldPaper instruction paper =
  let
    -- TODO: Type inference gets bananas here, not sure how to fix it yet
    -- accessor :: Lens' Coord Int = getAccessor instruction
    position = getPosition instruction
    pointsToFold =
      HashSet.filter (toFold (getAccessor instruction) position) paper
    foldedPoints =
      HashSet.map (foldPoint (getAccessor instruction) position) pointsToFold
  in
    HashSet.union foldedPoints (HashSet.difference paper pointsToFold)

getAccessor :: Instruction -> Lens' Coord Int
getAccessor (FoldX _) = _1
getAccessor (FoldY _) = _2

getPosition :: Instruction -> Int
getPosition (FoldX n) = n
getPosition (FoldY n) = n

toFold :: Lens' Coord Int -> Int -> Coord -> Bool
toFold accessor n = (> n) . view accessor

foldPoint :: Lens' Coord Int -> Int -> Coord -> Coord
foldPoint accessor n = over accessor (2*n -)

performFolds :: HashSet Coord -> [Instruction] -> HashSet Coord
performFolds = foldl' (flip foldPaper)

solver1 :: Parsed -> Int
solver1 (coords, folds) =
  HashSet.size . foldPaper (head folds) $ HashSet.fromList coords

solver2 :: Parsed -> Text
solver2 (coords, folds) = showPaper $ performFolds (HashSet.fromList coords) folds

showPaper :: HashSet Coord -> Text
showPaper paper =
  let
    bidim = Map.fromList $ zip (HashSet.toList paper) (repeat True)
    showCell (Just True) = "#"
    showCell _           = " "
  in
    showBidim showCell bidim

-- TODO we need a generic solver that allows for IO actions
main :: IO ()
main = do
  input <- getParsedInput day parser

  putStr "Solution 1: "
  print . solver1 $ input

  putStrLn "Solution 2: "
  putStrLn . solver2 $ input

