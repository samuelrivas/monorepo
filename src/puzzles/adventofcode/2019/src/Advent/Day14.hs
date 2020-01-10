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

module Advent.Day14 where

import           Prelude              hiding (lines, readFile, unlines)

import           Control.Lens         (at, non, over, set, traverse, view, _1,
                                       _2)
import           Data.Foldable        (foldl')
import           Data.Generics.Labels ()
import           Data.Graph
import           Data.HashMap.Strict  (HashMap, fromList, singleton, unionWith)
import qualified Data.HashMap.Strict  as HashMap
import           Data.Maybe           (fromMaybe)
import           Data.Ratio           ((%))
import           Data.Text            (Text, intercalate, lines, splitOn,
                                       unpack)
import qualified Data.Text            as Text
import           Data.Tuple           (swap)

import           System.IO.Advent     (getInput)

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

input_to_reactions :: [([Amount], Amount)] -> Reactions
input_to_reactions =
  let to_entry (amounts, (n, element)) = (element, (n, amounts))
  in fromList . fmap to_entry

get_input :: IO [([Amount], Amount)]
get_input = parse_input <$> getInput "14"

-- How many chemicals we need to produce something?
needs :: Reactions -> Amount -> HashMap Text Int
needs reactions (n, chemical) =
  fromMaybe mempty $ do
    (production, needed) <- view (at chemical) reactions
    let multiplier :: Int = ceiling $ n % production
    Just . fromList . fmap swap $ over (traverse . _1) (* multiplier) needed

-- given an inventory of chemicals, "unreact" one of them. That one is removed
-- and the reactors needed to create the amount we had of it are added to the
-- inventory
unreact :: Reactions -> HashMap Text Int -> Text -> HashMap Text Int
unreact reactions inventory chemical =
  let
    amount = view (at chemical . non 0) inventory
    wanted = needs reactions (amount, chemical)
  in
    unionWith (+) wanted (set (at chemical) Nothing inventory)

-- Unreact in chain until only ORE is left. This needs to be done in topological
-- order for correction, otherwise whe need to account for leftovers from
-- previous "unreactions" We assume that "FUEL" is first and "ORE" is last
unreaction_chain :: Reactions -> [Text] -> Int -> HashMap Text Int
unreaction_chain reactions toposort n =
  foldl' (unreact reactions) (singleton "FUEL" n) toposort

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

ore_for_n_fuel :: [([Amount], Amount)] -> Int -> Int
ore_for_n_fuel input n =
  let
    reactions = input_to_reactions input
    sorted = topological reactions
    final = unreaction_chain reactions sorted n
  in
    view (at "ORE" . non 0) final

bin_search_open :: (Int -> Ordering) -> Int -> Int -> (Int, Int)
bin_search_open p start current =
  case p current of
    EQ -> (current, current)
    LT -> bin_search_open p start (current * 2)
    GT -> bin_search p start current

bin_search :: (Int -> Ordering) -> Int -> Int -> (Int, Int)
bin_search p low high =
  let
    test = low + ((high - low) `div` 2)
  in
    if low > high
    then swap (low, high)
    else case p test of
           EQ -> (test, test)
           LT -> bin_search p (test + 1) high
           GT -> bin_search p low (test - 1)

find_how_much_fuel :: [([Amount], Amount)] -> Int -> Int
find_how_much_fuel input fuel =
  let p = flip compare fuel . ore_for_n_fuel input
  in view _1 $ bin_search_open p 1 1

solve_1 :: [([Amount], Amount)] -> Int
solve_1 = flip ore_for_n_fuel 1

solve_2 :: [([Amount], Amount)] -> Int
solve_2 = flip find_how_much_fuel 1000000000000

main :: IO ()
main = do
  input <- get_input
  putStrLn $ "Solution 1: " <> show (solve_1 input)
  putStrLn $ "Solution 2: " <> show (solve_2 input)
