{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

import           Prelude               hiding (getLine, lines, putStrLn,
                                        unlines, readFile)

import           Control.Lens          (assign, ix, modifying, preview, use,
                                        uses, toListOf, both, over)
import           Control.Monad         (when, join)
import           Control.Monad.Loops   (whileM_)
import           Control.Monad.Reader  (MonadReader)
import           Control.Monad.RWS.CPS (RWST, evalRWST, execRWST, get, put,
                                        runRWST, tell)
import           Control.Monad.State   (MonadState)
import           Control.Monad.Writer  (MonadWriter)
import           Data.Generics.Labels  ()
import           Data.List             (uncons)
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text, lines, pack, splitOn, unlines,
                                        unpack)
import           Data.Text.IO          (getLine, putStrLn, readFile)
import Data.Graph
import Data.Map.Strict ((!), Map, findMin, findMax, elems)
import Data.Set (Set, notMember)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Tuple (swap)
import Data.Foldable (find)
import qualified Data.Array as Array
import Data.Maybe (isJust, fromJust)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- The graph library requires us to map objects to Ints and back, so here we go
type Index = (Map Int Text, Map Text Int)

test_input :: Text
test_input =
  unlines
  ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L"]

test_input_2 :: Text
test_input_2 =
  unlines
  ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L",
   "K)YOU", "I)SAN"]

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
    vertices' = elems $ snd idx
    reachables = reachable g <$> vertices'
  in
    sum $ (+ (-1)) . length <$> reachables

solution_1 :: [(Text, Text)] -> Int
solution_1 orbits =
  let
    idx = get_index orbits
    g = get_graph orbits idx
  in
    count_orbits idx g

-- DFS until we reach destination
path' :: Graph -> Int -> Int -> Maybe [Int]
path' g orig dest = aux_path g orig dest [] Set.empty

aux_path :: Graph -> Int -> Int -> [Int] -> Set Int -> Maybe [Int]
aux_path g v dest acc visited
  | v == dest = Just (reverse $ dest:acc)
  | otherwise =
    join $ find isJust $ fmap (\v' -> aux_path g v' dest (v:acc) (Set.insert v visited))
     $ filter (`notMember` visited) (g Array.! v)

solution_2 :: [(Text, Text)] -> Maybe Int
solution_2 orbits =
  let
    idx = get_index orbits
    g = get_undirected_graph orbits idx
    orig = to_vertex idx "YOU"
    dest = to_vertex idx "SAN"

  in
    (+ (-3)) . length  <$> path' g orig dest

main :: IO ()
main = do
  Just orbits <- parse_input <$> readFile "input.txt"
  putStrLn $ "Solution 1: " <> (pack . show . solution_1 $ orbits)
  putStrLn $ "Solution 2: " <> (pack . show . fromJust . solution_2 $ orbits)
  
