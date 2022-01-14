{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-# LANGUAGE DeriveGeneric       #-}
module Advent.Day9 where

import           Perlude

import           Control.Lens         (At (at), _Just, filtered, folded,
                                       preview, sumOf, to, toListOf, view)
import           Data.Advent          (Day (..))
import           Data.Bidim           (Bidim, Coord, cross)
import           Data.Generics.Labels ()
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HashSet
import           Data.List            (sortBy)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (catMaybes, fromJust)
import           Data.Text            (intercalate)
import           System.IO.Advent     (getInput, solve)
import           Text.Parsec.Bidim    (bidim)
import           Text.Parsec.Parselib (Parser, unsafeParseAll)


type Parsed = Bidim Int

day :: Day
day = D9

rawInput :: IO Text
rawInput = getInput day

example :: Text
example = intercalate "\n"
  ["2199943210",
   "3987894921",
   "9856789892",
   "8767896789",
   "9899965678"
  ]

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parser :: Parser Parsed
parser = bidim (subtract 48 . fromEnum)

getHeight :: Bidim Int -> Coord -> Maybe Int
getHeight =  flip (view . at)

isLowPoint :: Bidim Int -> Coord -> Bool
isLowPoint floorMap coord =
  let
    getValue = getHeight floorMap
    positionValue = fromJust $ getValue coord
    neighbours = catMaybes $ getValue <$> cross coord
  in
    all (> positionValue) neighbours

lowPositions :: Bidim Int -> [Coord]
lowPositions floorMap =
  toListOf (traverse . filtered (isLowPoint floorMap)) $ Map.keys floorMap

riskLevel :: Bidim Int -> Int
riskLevel floorMap =
    sumOf (folded . _Just . to (+1))  (getHeight floorMap <$> lowPositions floorMap)

-- TODO This is something that we should abstract. MonadSearch may work here,
-- but it can also be overkill
growBasin :: Bidim Int -> [Coord] -> HashSet Coord -> HashSet Coord
growBasin _floorMap [] currentBasin = currentBasin
growBasin floorMap (x:candidates) currentBasin =
  let
    neighbours = filter (validCandidate floorMap) (cross x)
    newBasin = HashSet.insert x currentBasin
    newCandidates = filter (not . (`HashSet.member` newBasin)) neighbours
  in
    growBasin floorMap (newCandidates ++ candidates) newBasin

validCandidate :: Bidim Int -> Coord -> Bool
validCandidate floorMap coord =
  preview (at coord . _Just. to (< 9)) floorMap == Just True

basinSizes :: Bidim Int -> [Int]
basinSizes floorMap =
  let
    getBasin coord = growBasin floorMap [coord] HashSet.empty
  in
    HashSet.size . getBasin <$> lowPositions floorMap

solver1 :: Parsed -> Int
solver1 = riskLevel

solver2 :: Parsed -> Int
solver2 = product . take 3 . sortBy (flip compare) . basinSizes

main :: IO ()
main = solve day parser solver1 solver2