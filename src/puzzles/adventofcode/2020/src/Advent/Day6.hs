-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Advent.Day6 where

import           Prelude              hiding (getLine, lines, putStrLn,
                                       readFile, unlines)

import           Control.Lens         (both, over, toListOf)
import           Control.Monad        (join)
import qualified Data.Array           as Array
import           Data.Foldable        (find)
import           Data.Generics.Labels ()
import           Data.Graph           (Graph, buildG, reachable)
import           Data.Map.Strict      (Map, elems, findMax, findMin, (!))
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromJust, isJust)
import           Data.Set             (Set, notMember)
import qualified Data.Set             as Set
import           Data.Text            (Text, lines, pack, splitOn, unlines)
import           Data.Text.IO         (putStrLn)
import           Data.Tuple           (swap)

import           System.IO.Advent     (getInput)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- The graph library requires us to map objects to Ints and back, so here we go
type Index = (Map Int Text, Map Text Int)

test_input :: Text
test_input =
  unlines
  ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G",
   "G)H", "D)I", "E)J", "J)K", "K)L"]

test_input_2 :: Text
test_input_2 =
  unlines
  ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G",
   "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"]

parse_orbit :: Text -> Maybe (Text, Text)
parse_orbit input = do
  [x, y] <- Just . splitOn ")" $ input
  pure (x, y)

parse_input :: Text -> Maybe [(Text, Text)]
parse_input input = sequence (parse_orbit <$> lines input)

get_index :: [(Text, Text)] -> Index
get_index orbits =
  let
    objects = Set.fromList . toListOf (traverse . both) $ orbits
    indexed = zip (Set.toList objects) [0..]
  in
    (Map.fromList (swap <$> indexed), Map.fromList indexed)

to_vertex :: Index -> Text -> Int
to_vertex (_, idx) = (idx !)

to_object :: Index -> Int -> Text
to_object (idx, _) = (idx !)

get_graph :: [(Text, Text)] -> Index -> Graph
get_graph orbits idx@(vertex_to_object, _) =
  let
    bounds = (fst $ findMin vertex_to_object, fst $ findMax vertex_to_object)
    edges' = swap <$> over (traverse . both) (to_vertex idx) orbits
  in
    buildG bounds edges'

get_undirected_graph :: [(Text, Text)] -> Index -> Graph
get_undirected_graph orbits idx@(vertex_to_object, _) =
  let
    bounds = (fst $ findMin vertex_to_object, fst $ findMax vertex_to_object)
    edges' = over (traverse . both) (to_vertex idx) orbits
  in
    buildG bounds (edges' ++ (swap <$> edges'))

count_orbits :: Index -> Graph -> Int
count_orbits idx g =
  let
    vertices = elems $ snd idx
    reachables = reachable g <$> vertices
  in
    -- Subtract one since an object counts as reachable from itself
    sum $ (+ (-1)) . length <$> reachables

solution_1 :: [(Text, Text)] -> Int
solution_1 orbits =
  let
    idx = get_index orbits
    g = get_graph orbits idx
  in
    count_orbits idx g

-- Find the shortest path from orig to dest, both included in the result
path :: Graph -> Int -> Int -> Maybe [Int]
path dest orig = aux_path [] Set.empty dest orig

-- dfs algorithm
aux_path :: [Int] -> Set Int -> Graph -> Int -> Int -> Maybe [Int]
aux_path acc visited g dest orig
  | orig == dest = Just (reverse $ dest:acc)
  | otherwise =
    let
      hops = filter (`notMember` visited) (g Array.! orig)
      sub_path = aux_path (orig:acc) (Set.insert orig visited) g dest
      paths = sub_path <$> hops
    in
      join $ find isJust paths

solution_2 :: [(Text, Text)] -> Maybe Int
solution_2 orbits =
  let
    idx = get_index orbits
    g = get_undirected_graph orbits idx
    orig = to_vertex idx "YOU"
    dest = to_vertex idx "SAN"
  in
    -- Subtract 3 as we are including but us and santa, plus there is one less
    -- transfer than elements in the rest of the path
    (+ (-3)) . length  <$> path g dest orig

main :: IO ()
main = do
  Just orbits <- parse_input <$> getInput "6"
  putStrLn $ "Solution 1: " <> (pack . show . solution_1 $ orbits)
  putStrLn $ "Solution 2: " <> (pack . show . fromJust . solution_2 $ orbits)
