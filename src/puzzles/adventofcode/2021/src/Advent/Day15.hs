{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day15 where

import           Perlude

import           Advent.Day15.Internal           (Node (..))

import           Advent.Templib                  (solveM)
import           Control.Lens                    (Getter, _1, _2, _Just, at,
                                                  both, non, over, singular,
                                                  sumOf, to, view, views)
import           Control.Monad.MonadEmit         (MonadEmit)
import           Control.Monad.MonadSearch.Astar (AstarConfig, mkConfig,
                                                  searchAstarT)
import           Control.Monad.Reader            (MonadReader)
import           Data.Advent                     (Day (..))
import           Data.Bidim                      (Bidim, Coord, boundaries,
                                                  cell, cross)
import           Data.Char                       (digitToInt)
import           Data.Generics.Labels            ()
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as HashMap
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Maybe                      (fromJust)
import           Data.Metrics                    (Metrics)
import           Data.Text                       (intercalate)
import           System.IO.Advent                (getInput, getParsedInput)
import           Text.Parsec.Bidim               (bidim)
import           Text.Parsec.Parselib            (Parser, unsafeParseAll)

type Parsed =  Bidim Int

day :: Day
day = D15

rawInput :: IO Text
rawInput = getInput day

example :: Text
example =
  intercalate
  "\n" [
  "1163751742",
  "1381373672",
  "2136511328",
  "3694931569",
  "7463417111",
  "1319128137",
  "1359912421",
  "3125421639",
  "1293138521",
  "2311944581"
  ]

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parsedInput :: MonadFail m => MonadIO m => m Parsed
parsedInput = getParsedInput day parser

parser :: Parser Parsed
parser = bidim digitToInt

astarConfig :: Bidim Int -> AstarConfig Int Node (HashMap Coord Int) (Bidim Int)
astarConfig = mkConfig h cost explode isGoal rememberNode seenNode

rememberNode ::
  MonadReader (Bidim Int) m
  => HashMap Coord Int -> Node -> m (HashMap Coord Int)
rememberNode nodeStore n =
  let
    coord = view (#path . hd) n
  in
    pure $
    HashMap.insertWith min coord (view #cost n) nodeStore

seenNode :: MonadReader (Bidim Int) m => HashMap Coord Int -> Node -> m Bool
seenNode nodeStore n =
  let
    coord = view (#path . hd) n
    minCost = view (at coord . non maxBound) nodeStore
  in
    pure $ view #cost n >= minCost

-- TODO Figure out if there is a head lens for NonEmpty
hd :: Getter (NonEmpty a) a
hd = to NonEmpty.head

-- TODO Remove all the duplication, part 1 and part 2 differ only on the map
-- they use, so it should be easy to collapse them into a single solution
-- parameterised on the map they use

-- TODO add Manhattan distance to bidim libraries
h :: MonadReader (Bidim Int) m => Node -> m Int
h node =
  let
    pos = views #path NonEmpty.head node
  in do
    (_, maxCoord) <- view boundaries
    pure $ sumOf both maxCoord - sumOf both pos
{-# SCC h #-}

-- TODO add manhattan distance to bidim libraries
h2 :: MonadReader (Bidim Int) m => Node -> m Int
h2 node =
  let
    pos = views #path NonEmpty.head node
  in do
    (_, maxCoord) <- boundaries2
    pure $ sumOf both maxCoord - sumOf both pos
{-# SCC h2 #-}

cost :: MonadReader (Bidim Int) m => Node -> m Int
cost = pure . view #cost

explode :: MonadReader (Bidim Int) m => Node -> m [Node]
explode node =
  do
    limits <- view boundaries
    let newPos =
          filter (inRange limits)
          $ cross (view (#path . hd) node)
    traverse (addHop node) newPos
{-# SCC explode #-}

explode2 :: MonadReader (Bidim Int) m => Node -> m [Node]
explode2 node =
  do
    limits <- boundaries2
    let newPos =
          filter (inRange limits)
          $ cross (view (#path . hd) node)
    traverse (addHop2 node) newPos
{-# SCC explode2 #-}

inRange :: (Coord, Coord) -> Coord -> Bool
inRange ((minX, minY), (maxX, maxY)) (x, y) =
  minX <= x && x <= maxX && minY <= y && y <= maxY
{-# SCC inRange #-}

addHop :: MonadReader (Bidim Int) m => Node -> Coord -> m Node
addHop node pos =
  do
    risk <- getRisk pos
    pure $
      over #path (pos `NonEmpty.cons`)
      . over #cost (+ risk)
      $ node

getRisk :: MonadReader (Bidim Int) m => Coord -> m Int
getRisk pos = view (cell pos . non 0)

addHop2 :: MonadReader (Bidim Int) m => Node -> Coord -> m Node
addHop2 node pos =
  do
    risk <- getRisk2 pos
    pure $
      over #path (pos `NonEmpty.cons`)
      . over #cost (+ risk)
      $ node

getRisk2 :: MonadReader (Bidim Int) m => Coord -> m Int
getRisk2 pos =
  do
    (basePos, addedRisk) <- mapExtendedPos pos
    baseRisk <- getRisk basePos
    pure $ (baseRisk + addedRisk - 1) `mod` 9 + 1

-- Return the position in the base map and the added risk
mapExtendedPos :: MonadReader (Bidim Int) m => Coord -> m (Coord, Int)
mapExtendedPos pos =
  do
    bds <- view boundaries
    let
      maxX = view (_2 . _1) bds + 1
      maxY = view (_2 . _2) bds + 1
      addX = view _1 pos `div` maxX
      addY = view _2 pos `div` maxY
      basePos = over _1 (`mod` maxX)
                . over _2 (`mod` maxY)
                $ pos

    pure (basePos, addX + addY)

isGoal :: MonadReader (Bidim Int) m => Node -> m Bool
isGoal node =
  let
    pos = view (#path . hd) node
  in
    (pos ==) . view _2 <$> view boundaries

initialNode :: Node
initialNode = Node ((0,0) :| []) 0

astarConfig2 :: Bidim Int -> AstarConfig Int Node (HashMap Coord Int) (Bidim Int)
astarConfig2 = mkConfig h2 cost explode2 isGoal2 rememberNode seenNode

boundaries2 :: MonadReader (Bidim Int) m => m (Coord, Coord)
boundaries2 = over (_2 . both) (subtract 1 . (*5) . (+ 1)) <$> view boundaries

isGoal2 :: MonadReader (Bidim Int) m => Node -> m Bool
isGoal2 node =
  let
    pos = view (#path . hd) node
  in
    (pos ==) . view _2 <$> boundaries2

-- TODO Add Astar and runAstar to Astar
-- TODO Maybe this can be faster with a Dijkstra MonadSearch
solver1 :: MonadEmit (Metrics Int Int) m => Parsed -> m Int
solver1 input = do
  (maybeNode, ()) <- searchAstarT (astarConfig input) initialNode HashMap.empty
  pure . view (singular _Just . #cost) $ maybeNode

solver2 :: MonadEmit (Metrics Int Int) m => Parsed -> m Int
solver2 input = do
  (maybeNode, ()) <- searchAstarT (astarConfig2 input) initialNode HashMap.empty
  pure . view (singular _Just . #cost) $ maybeNode

main :: IO ()
main = solveM day parser solver1 solver2
