{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day7 where

import           Advent.Perlude

import           Control.Lens          (at, both, each, foldlOf, non, over,
                                        view, _2)
import           Control.Monad         (guard)
import           Data.Foldable         (fold)
import           Data.Functor          (($>))
import           Data.Graph            (Graph, Vertex, graphFromEdges)
import           Data.List             (find, foldl', sort, unfoldr)
import           Data.Map              (Map, assocs, keysSet)
import qualified Data.Map              as Map
import           Data.Maybe            (fromJust)
import           Data.Maybe            (isJust)
import           Data.Set              (Set, difference, member)
import qualified Data.Set              as Set
import           Data.Text             (Text, count, dropEnd, lines, pack,
                                        replace, singleton, splitOn, stripEnd,
                                        takeEnd, unpack)
import qualified Data.Text             as Text
import qualified System.IO.Advent      as IOAdvent
import           Text.Parsec.Text      (Parser)
import qualified Text.Read             as Read

-- TODO: Close
import           Advent.Templib.Parsec hiding (parse)
import           Text.Parsec           hiding (getInput, parse)


import           Advent.Templib        (Day (..), getInput', getParsedInput)

day :: Day
day = D7

-- TODO: We need a better graph abstraction. Using a map like here seems to be
-- more convenient than Data.Graph, but then we need to implement all
-- interesting algorithms ourselves. There is surely an alternative ready to use

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
    bags = parseContained `sepBy` string ", " <* char '.'
    noBags = string "no other bags." $> []
  in
    (,)
    <$> parseColour <* string " bags contain "
    <*> (bags <|> noBags)

parseColour :: Parser Text
parseColour =
  fold [
    text1 (noneOf " "),
    singleton <$> char ' ', -- TODO: Define literal :: Text -> Parser Text
    text1 (noneOf " ")
  ]

parseContained :: Parser (Int, Text)
parseContained =
  (,)
  <$> digitsAsNum <* string " "
  <*> parseColour <* string " bag" <* optional (char 's')

getInput :: IO Text
getInput = getInput' day

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
