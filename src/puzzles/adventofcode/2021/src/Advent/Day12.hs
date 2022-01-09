{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- TODO: There is an abstraction missing here, either for MonadSearch or
-- something related to graph traversal (but we need to remember the path here to
-- know which routes are valid)

module Advent.Day12 where

import           Perlude


import           Advent.Day12.Internal
import           Advent.Templib        (linesOf)

import           Control.Lens          (at, filtered, non, over, set, toListOf,
                                        view, views)
import           Data.Advent           (Day (..))
import           Data.Char             (isLower)
import           Data.Foldable         (foldl')
import           Data.Generics.Labels  ()
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HashMap
import           Data.HashSet          (HashSet)
import qualified Data.HashSet          as HashSet
import           Data.List.NonEmpty    (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty    as NonEmpty
import           Data.Maybe            (fromJust)
import           Data.Text             (intercalate)
import qualified Data.Text             as Text
import           System.IO.Advent      (getInput, solve)
import           Text.Parsec           (noneOf)
import           Text.Parsec.Parselib  (Parser, literal, text1, unsafeParseAll)

-- TODO This problem may be solvable just using parsec
type Parsed = [(Text, Text)]

day :: Day
day = D12

rawInput :: IO Text
rawInput = getInput day

example :: Text
example =
  intercalate "\n" [
  "start-A",
  "start-b",
  "A-c",
  "A-b",
  "b-d",
  "A-end",
  "b-end"
  ]

example2 :: Text
example2 =
  intercalate "\n" [
  "dc-end",
  "HN-start",
  "start-kj",
  "dc-start",
  "dc-HN",
  "LN-dc",
  "HN-end",
  "kj-sa",
  "kj-HN",
  "kj-dc"
  ]

example3 :: Text
example3 =
  intercalate "\n" [
  "fs-end",
  "he-DX",
  "fs-he",
  "start-DX",
  "pj-DX",
  "end-zg",
  "zg-sl",
  "zg-pj",
  "pj-he",
  "RW-he",
  "fs-DX",
  "pj-RW",
  "zg-RW",
  "start-pj",
  "he-WI",
  "zg-he",
  "pj-fs",
  "start-RW"
  ]

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parsedExample2 :: Parsed
parsedExample2 = fromJust $ unsafeParseAll parser example2

parsedExample3 :: Parsed
parsedExample3 = fromJust $ unsafeParseAll parser example3

parser :: Parser Parsed
parser = linesOf connectionP

connectionP :: Parser (Text, Text)
connectionP =
  (,)
  <$> text1 (noneOf "-\n") <* literal "-"
  <*> text1 (noneOf "-\n")

type Graph = HashMap Text (HashSet Text)

-- This is a digraph, but we remove the connections to "start" and from "end" as
-- they are never going to create a valid route
toGraph :: Parsed -> Graph
toGraph connections =
  let
    -- TODO This is too copy-pasty, fix it
    toPairs ("start", y) = [("start", HashSet.singleton y)]
    toPairs (x, "start") = [("start", HashSet.singleton x)]
    toPairs ("end", y)   = [(y, HashSet.singleton "end")]
    toPairs (x, "end")   = [(x, HashSet.singleton "end")]
    toPairs (x, y)       = [(x, HashSet.singleton y), (y, HashSet.singleton x)]
    add g connection =
      HashMap.unionWith HashSet.union g $
      HashMap.fromList (toPairs connection)
  in
    foldl' add HashMap.empty connections

isSmall :: Text -> Bool
isSmall = isLower . Text.head

lastVisited :: Path -> Text
lastVisited = views #route NonEmpty.head

validHops :: Graph -> Path -> HashSet Text
validHops g p =
  let
    current = lastVisited p
    connected = view (at current . non HashSet.empty) g
    valid dest =
      null (view #smallCave p) || not (HashSet.member dest (view #smallCaves p))
  in
    HashSet.filter valid connected

-- Adds the cave to the wildcard if it is small and have been visited already
addHop :: Path -> Text -> Path
addHop p hop =
  let
    wildcard =
      if isSmall hop && HashSet.member hop (view #smallCaves p)
        then Just hop
        else view #smallCave p
  in
    prependToRoute (set #smallCave wildcard p) hop

-- Adds the cave to the front of the route, and remembers it if it was small
prependToRoute :: Path -> Text -> Path
prependToRoute p hop =
  let
    smallHop = if isSmall hop then HashSet.singleton hop else HashSet.empty
  in
    over #route (hop <|)
    . over #smallCaves (HashSet.union smallHop)
    $ p

initialRoute :: Path
initialRoute = Path ("start" :| []) (HashSet.singleton "start") (Just "taken")

initialRoute2 :: Path
initialRoute2 = Path ("start" :| []) (HashSet.singleton "start") Nothing

explode :: Graph -> Path -> [Path]
explode graph path = addHop path <$> HashSet.toList (validHops graph path)

findAllPaths :: Graph -> [Path] -> [Path] -> [Path]
findAllPaths _graph [] closed = closed
findAllPaths graph (h:t) closed =
  let
    exploded = explode graph h
  in
    if null exploded
    then findAllPaths graph t (h:closed)
    else findAllPaths graph (exploded ++ t) closed

solver :: Path -> Parsed -> Int
solver initial input =
  length
  $ toListOf (traverse . #route . filtered ((== "end") . NonEmpty.head))
  $ findAllPaths (toGraph input) [initial] []

solver1 :: Parsed -> Int
solver1 = solver initialRoute

solver2 :: Parsed -> Int
solver2 = solver initialRoute2

main :: IO ()
main = solve day parser solver1 solver2
