{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DeriveGeneric       #-}
module Advent.Day8 where

import           Perlude

import           Advent.Templib                (linesOf)
import           Control.Lens                  (_2, to, toListOf)
import           Data.Advent                   (Day (..))
import           Data.Functor                  (($>))
import           Data.Generics.Labels          ()
import           Data.Hashable                 (Hashable (..))
import           Data.Hashable.Generic         (genericHashWithSalt)
import           Data.HashSet                  (HashSet)
import qualified Data.HashSet                  as HashSet
import           Data.Maybe                    (fromJust)
import           Data.Text                     (intercalate)
import           GHC.Generics                  (Generic)
import           System.IO.Advent              (getInput, solve)
import           Text.Parsec                   (char, sepBy, sepEndBy, (<|>))
import           Text.Parsec.Parselib          (Parser, literal, unsafeParseAll)
import           Text.ParserCombinators.Parsec (many1)

data Wire = A | B | C | D | E | F | G deriving stock (Show, Eq, Generic)
instance Hashable Wire where
  hashWithSalt = genericHashWithSalt

type Parsed = [([HashSet Wire], [HashSet Wire])]

day :: Day
day = D8

rawInput :: IO Text
rawInput = getInput day

example :: Text
example = intercalate "\n"
  ["be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
   "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
   "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
   "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
   "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
   "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
   "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
   "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
   "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
   "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
  ]

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parser :: Parser Parsed
parser = linesOf entryParser

entryParser :: Parser ([HashSet Wire], [HashSet Wire])
entryParser =
  (,)
  <$> (patternParser `sepEndBy` char ' '  <* literal "| ")
  <*> patternParser `sepBy` char ' '

patternParser :: Parser (HashSet Wire)
patternParser = HashSet.fromList <$> many1 wireParser

wireParser :: Parser Wire
wireParser =
  char 'a' $>  A
  <|> char 'b' $>  B
  <|> char 'c' $>  C
  <|> char 'd' $>  D
  <|> char 'e' $>  E
  <|> char 'f' $>  F
  <|> char 'g' $>  G

-- use filtered lens to shorten this
solver1 :: Parsed -> Int
solver1 = length . filter (`elem` [2, 3, 4, 7]) . toListOf (traverse . _2 . traverse . to HashSet.size)

solver2 :: Parsed -> Int
solver2 = undefined

main :: IO ()
main = solve day parser solver1 solver2
