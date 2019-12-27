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
{-# LANGUAGE TupleSections         #-}

import           Prelude              hiding (lines, putStrLn, readFile, show,
                                       unlines)
import qualified Prelude

import           Control.Monad.State  (StateT, evalStateT, gets, modify)
import           Data.Bool            (bool)
import           Data.Foldable        (foldl')
import           Data.Generics.Labels ()
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HashSet
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text, pack)
import           Data.Text.IO         (putStrLn, readFile)

import           Bidim

show :: Show a => a -> Text
show = pack . Prelude.show

solve1 :: Text -> IO ()
solve1 text =
  let eris = parseInput text
  in do
    dup <- evalStateT (findDup eris) HashSet.empty
    putStrLn $ "Solution 1: " <> show (bioDiv dup)

bioDiv :: Bidim Bool -> Integer
bioDiv bidim =
  let
    index (x,y) = x + 5 * y
    cellBioDiv total pos True   = total + 2 ^ index pos
    cellBioDiv total _pos False = total
  in
    Map.foldlWithKey cellBioDiv 0 bidim

solve2 :: Text -> IO ()
solve2 text =
  let
    hyperEris = getHyperEris text
    infested = foldl' (.) id (replicate 200 hyperMinute) hyperEris
    count = Map.foldl (flip $ (+) . fromEnum) 0 infested
  in
    putStrLn $ "Solution 2: " <> show count

getInput :: IO Text
getInput = readFile "input.txt"

parseInput :: Text -> Bidim Bool
parseInput = Map.map (== '#') . fromText

showCell :: Maybe Bool -> Text
showCell = maybe "?" (bool "." "#")

readCell :: Ord a => Map a Bool -> a -> Bool
readCell eris p = Map.findWithDefault False p eris

updateCell :: Bidim Bool -> Coord -> Bool -> Bool
updateCell eris pos isBug =
  let
    neighbors = readCell eris <$> cross pos
    numBugs = sum $ fromEnum <$> neighbors
  in
    case (numBugs, isBug) of
      (n, True) | n /= 1            -> False
      (n, False) | 1 <= n && n <= 2 -> True
      (_, _)                        -> isBug

updateHyperCell :: HyperEris -> HyperCoord -> Bool -> Bool
updateHyperCell _ ((2, 2), _) _ = False
updateHyperCell eris pos isBug =
  let
    neighbors = readCell eris <$> hyperCross pos
    numBugs = sum $ fromEnum <$> neighbors
  in
    case (numBugs, isBug) of
      (n, True) | n /= 1            -> False
      (n, False) | 1 <= n && n <= 2 -> True
      (_, _)                        -> isBug

minute :: Bidim Bool -> Bidim Bool
minute eris = Map.mapWithKey (updateCell eris) eris

hyperMinute :: HyperEris -> HyperEris
hyperMinute eris = Map.mapWithKey (updateHyperCell eris) eris

-- For debugging
project :: Integer -> HyperEris -> Bidim Bool
project level hyperEris =
  let onlyLevel hyperCoord _ = snd hyperCoord == level
  in Map.mapKeys fst $ Map.filterWithKey onlyLevel hyperEris

inject :: Integer -> Bidim Bool -> HyperEris
inject level = Map.mapKeys (, level)

type ErisT = StateT (HashSet Text)

-- Coord and level
type HyperCoord = (Coord, Integer)

type HyperEris = Map HyperCoord Bool

inBounds :: Coord -> Bool
inBounds coord@(x, y) =
  let
    valid a = (a >= 0) && (4 >= a)
    center = (2, 2)
  in
    valid x && valid y && coord /= center

toHyperCoord :: Integer -> Coord -> HyperCoord
toHyperCoord level coord = (coord, level)

hyperCross :: HyperCoord -> [HyperCoord]
hyperCross hyperCoord@(coord, level) =
  let
    fromLevel  = toHyperCoord level <$> filter inBounds (cross coord)
  in
    HashSet.toList . HashSet.fromList .
    concat $ [
      fromLevel,
      innerTop hyperCoord,
      innerLeft hyperCoord,
      innerBottom hyperCoord,
      innerRight hyperCoord,
      outerTop hyperCoord,
      outerLeft hyperCoord,
      outerBottom hyperCoord,
      outerRight hyperCoord
      ]

innerTop :: HyperCoord -> [HyperCoord]
innerTop ((2, 1), level) = (, level + 1) . (, 0) <$> [0..4]
innerTop _               = []

innerBottom :: HyperCoord -> [HyperCoord]
innerBottom ((2, 3), level) = (, level + 1) . (, 4) <$> [0..4]
innerBottom _               = []

innerLeft :: HyperCoord -> [HyperCoord]
innerLeft ((1, 2), level) = (, level + 1) . (0, ) <$> [0..4]
innerLeft _               = []

innerRight :: HyperCoord -> [HyperCoord]
innerRight ((3, 2), level) = (, level + 1) . (4, ) <$> [0..4]
innerRight _               = []

outerTop :: HyperCoord -> [HyperCoord]
outerTop ((_, 0), level) = [((2, 1), level - 1)]
outerTop _               = []

outerBottom :: HyperCoord -> [HyperCoord]
outerBottom ((_, 4), level) = [((2, 3), level - 1)]
outerBottom _               = []

outerLeft :: HyperCoord -> [HyperCoord]
outerLeft ((0, _), level) = [((1, 2), level - 1)]
outerLeft _               = []

outerRight :: HyperCoord -> [HyperCoord]
outerRight ((4, _), level) = [((3, 2), level - 1)]
outerRight _               = []

emptyHyperEris :: HyperEris
emptyHyperEris =
  Map.fromList [(((x, y), l), False) | x <- [0..4], y <- [0..4], l <- [-101..101]]

getHyperEris :: Text -> HyperEris
getHyperEris text = Map.union (inject 0 . parseInput $ text) emptyHyperEris

findDup :: Monad m => Bidim Bool -> ErisT m (Bidim Bool)
findDup eris =
  let
    printedEris = showBidim showCell eris
  in do
    seen <- gets $ HashSet.member printedEris
    if seen
      then pure eris
      else do
        modify (HashSet.insert printedEris)
        findDup (minute eris)

main :: IO ()
main = do
  input <- getInput
  solve1 input
  solve2 input
