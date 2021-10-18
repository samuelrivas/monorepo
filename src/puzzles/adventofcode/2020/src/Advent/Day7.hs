{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day7 where

import           Perlude

import           Advent.Templib.Parsec (digitsAsNum, literal, text1)
import           Control.Lens          (at, non, view)
import           Data.Foldable         (fold, foldl')
import           Data.Functor          (($>))
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Text.Parsec           (char, noneOf, optional, sepBy, sepEndBy,
                                        (<|>))
import           Text.Parsec.Text      (Parser)

import           Advent.Templib        (Day (..), getInput', getParsedInput)

-- TODO: We need a better graph abstraction. Using a map like here seems to be
-- more convenient than Data.Graph, but then we need to implement all
-- interesting algorithms ourselves. There is surely an alternative ready to use

day :: Day
day = D7

getInput :: IO Text
getInput = getInput' day

example :: Text
example = "light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
          \dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
          \bright white bags contain 1 shiny gold bag.\n\
          \muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
          \shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
          \dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
          \vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
          \faded blue bags contain no other bags.\n\
          \dotted black bags contain no other bags.\n"

-- Each rule is encoded as (container colour, [(amount, contained colour)])
parser :: Parser [(Text, [(Int, Text)])]
parser = parseLine `sepEndBy` char '\n'

parseLine :: Parser (Text, [(Int, Text)])
parseLine =
  let
    bags = parseContained `sepBy` literal ", " <* char '.'
    noBags = literal "no other bags." $> []
  in
    (,)
    <$> parseColour <* literal " bags contain "
    <*> (bags <|> noBags)

parseColour :: Parser Text
parseColour =
  fold [
    text1 (noneOf " "),
    literal " ",
    text1 (noneOf " ")
  ]

parseContained :: Parser (Int, Text)
parseContained =
  (,)
  <$> digitsAsNum <* literal " "
  <*> parseColour <* literal " bag" <* optional (char 's')

toAssocs :: (Text, [(Int, Text)]) -> [(Text, [Text])]
toAssocs (outer, inner) =
  let toAssoc (_, colour) = (colour, [outer])
  in toAssoc <$> inner

toGraph :: [(Text, [(Int, Text)])] -> Map Text [Text]
toGraph rules = Map.unionsWith (++) $ Map.fromListWith (++) . toAssocs <$> rules

-- TODO: This is a very ugly way to get all reachable nodes. And it will hang if
-- there are cycles
reachable :: Map Text [Text] -> Set Text -> Set Text
reachable graph acc =
  if Set.null acc then Set.empty
  else
    let
      node = Set.elemAt 0 acc
      fromNode = Set.fromList $ view (at node . non []) graph
      newAcc = Set.union (Set.delete node acc) fromNode
    in
      Set.union fromNode (reachable graph newAcc)

bagsInside :: Text -> [(Text, [(Int, Text)])] -> Int
bagsInside colour rules =
  let
    ruleMap = Map.fromList rules
    next = view (at colour . non []) ruleMap
    f acc (n, colour') = acc + n + n * bagsInside colour' rules
  in
    foldl' f 0 next

solution1 :: Map Text [Text] -> Int
solution1 graph = Set.size . reachable graph . Set.fromList $ ["shiny gold"]

solution2 :: [(Text, [(Int, Text)])] -> Int
solution2 = bagsInside "shiny gold"

main :: IO ()
main = do
  input <- getParsedInput day parser

  putStr "Solution 1: "
  print . solution1 . toGraph $ input

  putStr "Solution 2: "
  print . solution2 $ input
