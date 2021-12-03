{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day2 where

import           Perlude

import           Control.Lens         (each, sumOf)
import           Data.Advent          (Day (..))
import           Data.Bidim           (Coord, plus)
import           Data.Foldable        (foldl')
import           System.IO.Advent     (getInput, solve)
import           Text.Parsec          (char, sepEndBy, (<|>))
import           Text.Parsec.Advent   (listOfNum)
import           Text.Parsec.Parselib (Parser, digitsAsNum, literal)

import           Advent.Templib       (linesOf)

day :: Day
day = D2

rawInput :: IO Text
rawInput = getInput day

parser :: Parser [Coord]
parser = linesOf instruction

instruction :: Parser Coord
instruction =
  literal "forward " *> ((,0) <$> digitsAsNum)
  <|> literal "down " *> ((0,) <$> digitsAsNum)
  <|> literal "up " *> ((0,) . negate  <$> digitsAsNum)

-- TODO I am pretty sure there is a prettier lens operation lurking around here.
-- TODO Coord really needs to be an instance of Sum, Num and perhaps Product
solver1 :: [Coord] -> Int
solver1 l =
  let (x, y) = foldl' plus (0,0) l
  in x * y

-- TODO This is rather ugly, probably we should not be using Coord but rather a
-- sum type for the instructions
solver2 :: [Coord] -> Int
solver2 l =
  let
    f (x,y,aim) (forward,v)
      | forward /= 0 = (x + forward, y + aim * forward, aim)
      | otherwise = (x, y, aim +v)
    (x, y, _) = foldl' f (0,0,0) l
  in
    x * y

main :: IO ()
main = solve day parser solver1 solver2

