-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import           Prelude              hiding (lines, readFile, unlines)

import           Control.Lens         (Getter, at, each, non, over, set, sumOf,
                                       to, toListOf, traverse, view, _1, _2, _3)
import           Control.Monad        (replicateM_)
import           Control.Monad.Loops  (whileM)
import           Control.Monad.State  (State, evalState, execState, get, modify)
import           Data.Foldable        (foldl')
import           Data.Generics.Labels ()
import           Data.Graph
import           Data.HashMap.Strict  (HashMap, fromList, singleton, unionWith)
import qualified Data.HashMap.Strict  as HashMap
import           Data.HashSet         (HashSet, member)
import qualified Data.HashSet         as HashSet
import           Data.Maybe           (fromMaybe)
import           Data.Ratio           ((%))
import           Data.Text            (Text, intercalate, lines, splitOn,
                                       stripPrefix, unpack, words)
import qualified Data.Text            as Text
import           Data.Text.IO         (readFile)
import           Data.Tuple           (swap)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

type Amount = (Int, Text)
type Reactions = HashMap Text (Int, [Amount])

example :: Text
example = intercalate "\n"
  ["10 ORE => 10 A",
   "1 ORE => 1 B",
   "7 A, 1 B => 1 C",
   "7 A, 1 C => 1 D",
   "7 A, 1 D => 1 E",
   "7 A, 1 E => 1 FUEL"
  ]

breakOn :: Text -> Text -> (Text, Text)
breakOn br = over _2 (Text.drop $ Text.length br) . Text.breakOn br

parse_amount :: Text -> Amount
parse_amount = over _1 (read . unpack) . breakOn " "

parse_line :: Text -> ([Amount], Amount)
parse_line line =
  let (amounts, amount) = over _1 (splitOn ", ") . breakOn " => " $ line
  in (parse_amount <$> amounts, parse_amount amount)

parse_input :: Text -> [([Amount], Amount)]
parse_input = fmap parse_line . lines

input_to_map :: [([Amount], Amount)] -> Reactions
input_to_map =
  let to_entry (amounts, (n, element)) = (element, (n, amounts))
  in fromList . fmap to_entry

get_input :: IO [([Amount], Amount)]
get_input = parse_input <$> readFile "input.txt"

-- XXX Needs to run in toposort
needs :: Reactions -> Amount -> HashMap Text Int
needs reactions (n, chemical) =
  fromMaybe mempty $ do
    (production, needed) <- view (at chemical) reactions
    let multiplier :: Int = ceiling $ n % production
    Just . fromList . fmap swap $ over (traverse . _1) (* multiplier) needed

produce :: Reactions -> HashMap Text Int -> Text -> HashMap Text Int
produce reactions inventory chemical =
  let
    amount = view (at chemical . non 0) inventory
    wanted = needs reactions (amount, chemical)
  in
    unionWith (+) wanted (set (at chemical) Nothing inventory)

unreact :: Reactions -> [Text] -> HashMap Text Int
unreact reactions toposort =
  foldl' (produce reactions) (singleton "FUEL" 1) toposort

to_graph ::
  Reactions -> (Graph, Vertex -> Text)
to_graph reactions =
  let
    to_edges :: (Text, (Int, [Amount])) -> (Text, Text, [Text])
    to_edges (chemical, (_, amounts)) =
      (chemical, chemical, view  _2 <$> amounts)
    (g, vs, _ks) = graphFromEdges (to_edges <$> HashMap.toList reactions)
  in (g, view _1 . vs)

topological :: Reactions -> [Text]
topological reactions =
  let (g, idx) = to_graph reactions
  in idx <$> topSort g

solve_1 :: [([Amount], Amount)] -> Int
solve_1 input =
  let
    reactions = input_to_map input
    sorted = topological reactions
    final = unreact reactions sorted
  in
    view (at "ORE" . non 0) final

main :: IO ()
main = do
  input <- get_input
  putStrLn $ "Solution 1: " <> show (solve_1 input )

