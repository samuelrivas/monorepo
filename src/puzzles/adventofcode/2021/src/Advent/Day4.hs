{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day4 where

import           Perlude

import           Data.Advent          (Day (..))
import           Data.Foldable        (foldl')
import           Data.Functor         (($>))
import           Data.List            (transpose)
import           Data.Text            (intercalate)
import           System.IO.Advent     (getInput, solve)
import           Text.Parsec          (char, many1, sepBy, sepEndBy, (<|>))
import           Text.Parsec.Parselib (Parser, digitsAsNum, literal)

import           Advent.Templib       (binToDec, linesOf, matrix)
import           Data.Bidim           (Bidim)
import           Text.Parsec.Bidim    (bidim)

type Parsed =  ([Int], [[[Int]]])

day :: Day
day = D4

rawInput :: IO Text
rawInput = getInput day

example :: Text
example = intercalate "\n" [
  "foo"]

--parser :: Parser Parsed
parser =
  (,)
  <$> digitsAsNum `sepBy` char ',' <* literal "\n\n"
  <*> linesOf (matrix digitsAsNum)

solver1 :: Parsed -> Int
solver1 l = undefined

solver2 :: Parsed -> Int
solver2 l = undefined

main :: IO ()
main = solve day parser solver1 solver2
