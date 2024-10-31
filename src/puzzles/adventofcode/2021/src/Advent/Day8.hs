{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day8 where

import           Perlude

import           Advent.Templib                (linesOf)
import           Control.Lens                  (_2, at, over, to, toListOf)
import           Data.Advent                   (Day (..))
import           Data.Functor                  (($>))
import           Data.Generics.Labels          ()
import           Data.Hashable                 (Hashable (..))
import           Data.Hashable.Generic         (genericHashWithSalt)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HashMap
import           Data.HashSet                  (HashSet)
import qualified Data.HashSet                  as HashSet
import           Data.Maybe                    (fromJust)
import           Data.Text                     (intercalate)
import           GHC.Generics                  (Generic)
import           System.IO.Advent              (getInput, solve)
import           Text.Parsec                   (char, sepBy, sepEndBy, (<|>))
import           Text.Parsec.Parselib          (Parser, literal, unsafeParseAll)
import           Text.ParserCombinators.Parsec (many1)

data Wire = A | B | C | D | E | F | G deriving stock (Show, Eq, Generic, Enum)
instance Hashable Wire where
  hashWithSalt = genericHashWithSalt

-- Each output can be mapped to multiple inputs
type PossibleConnections = HashMap Wire (HashSet Wire)

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

allWires :: HashSet Wire
allWires = HashSet.fromList [A .. G]

initialState :: PossibleConnections
initialState = HashMap.fromList $ (, allWires) <$> [A .. G]

constrainOutput :: Wire -> HashSet Wire -> PossibleConnections -> PossibleConnections
constrainOutput w ws = over (at w) (fmap $ HashSet.intersection ws)

negateWires :: HashSet Wire -> HashSet Wire
negateWires = HashSet.difference allWires

-- Return the inputs that have been matched to a single output
solvedInputs :: PossibleConnections -> [Wire]
solvedInputs = HashMap.keys . HashMap.filter ((== 1) . HashSet.size)

-- Given a set of inputs, get the outputs that can be constrained by that input
-- set and the outputs that can be constrained as the negation of that same
-- input set.
constrainSet :: HashSet Wire -> ([Wire], [Wire])
constrainSet inputs =
  case HashSet.size inputs of
    6 -> ([A, B, F, G], []) -- 0, 6, 9
    2 -> ([C, F], [A, B, D, E, G]) -- 1
    5 -> ([A, D, G], []) -- 2, 3, 5
    4 -> ([B, C, D, F], [A, E, G]) -- 4
    3 -> ([A, C, F], [B, D, E, G]) -- 7
    7 -> ([A .. G], []) -- 8
    _ -> error $ "invalid input set:" <> show inputs

-- use filtered lens to shorten this
solver1 :: Parsed -> Int
solver1 = length . filter (`elem` [2, 3, 4, 7]) . toListOf (traverse . _2 . traverse . to HashSet.size)

solver2 :: Parsed -> Int
solver2 = undefined

main :: IO ()
main = solve day parser solver1 solver2
