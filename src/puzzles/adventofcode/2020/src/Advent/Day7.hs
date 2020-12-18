{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day7 where

import           Prelude          hiding (lines, putStr, putStrLn, read, show)
import qualified Prelude

import           Control.Lens     (at, both, each, foldlOf, non, over, view, _2)
import           Control.Monad    (guard)
import           Data.Graph       (Graph, Vertex, graphFromEdges)
import           Data.List        (find, foldl', sort, unfoldr)
import           Data.Map         (Map, assocs, keysSet)
import qualified Data.Map         as Map
import           Data.Maybe       (fromJust)
import           Data.Maybe       (isJust)
import           Data.Set         (Set, difference, member)
import qualified Data.Set         as Set
import           Data.Text        (Text, count, dropEnd, lines, pack, replace,
                                   singleton, splitOn, stripEnd, takeEnd,
                                   unpack)
import qualified Data.Text        as Text
import           Data.Text.IO     (putStr, putStrLn)
import qualified System.IO.Advent as IOAdvent
import qualified Text.Read        as Read

-- TODO: We need a better graph abstraction. Using a map like here seems to be
-- more convenient than Data.Graph, but then we need to implement all
-- interesting algorithms ourselves. There is surely an alternative ready to use

-- TODO: Move read :: Text -> a to our own prelude
read :: Read a => Text -> a
read = Prelude.read . unpack

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

toTuple :: [a] -> Maybe (a, a)
toTuple [a, b] = Just (a, b)
toTuple _      = Nothing

splitRules :: Text -> Maybe [(Text, Text)]
splitRules = traverse (toTuple . splitOn " bags contain ") . lines

parseBody :: Text -> [(Int, Text)]
parseBody "no other bags." = []
parseBody t =
  parseBagSpec <$> (splitOn ", " . dropEnd 1 $ t)

parseBagSpec :: Text -> (Int, Text)
parseBagSpec spec =
  let
    (amount, colour) = Text.break (== ' ') spec
  in
    -- TODO: This is horrible, I really need to start using parsec for these
    -- things or at least regexes
    (read amount, Text.strip . Text.dropEnd 1 . Text.dropWhileEnd (/= ' ' ) $ colour)

-- Each rule is encoded as (container colour, [(amount, contained colour)])
parse :: Text -> Maybe [(Text, [(Int, Text)])]
parse text = do
  rawRules <- splitRules text
  return $ over _2 parseBody <$> rawRules

getInput :: IO Text
getInput = IOAdvent.getInput "7"

toAssocs :: (Text, [(Int, Text)]) -> [(Text, [Text])]
toAssocs (outer, inner) =
  let toAssoc (_, colour) = (colour, [outer])
  in toAssoc <$> inner

toGraph :: [(Text, [(Int, Text)])] -> Map Text [Text]
toGraph rules = Map.unionsWith (++) $ Map.fromListWith (++) . toAssocs <$> rules

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

main :: IO ()
main = do
  input <- getInput

  putStr "Solution 1: "
  print $ "foo"

  putStr "Solution 2: "
  print $ "foo"
