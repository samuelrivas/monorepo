{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Advent.Day15 where

import           Perlude

import           Advent.Day15.Internal           (Node (..))

import           Advent.Templib                  (Metrics, MonadEmit, solveM)
import           Control.Lens                    (Getter, _1, _2, _Just, at,
                                                  both, non, over, singular,
                                                  sumOf, to, view, views)
import           Control.Monad.MonadSearch.Astar (AstarConfig, mkConfig,
                                                  searchAstarT)
import           Control.Monad.Reader            (MonadReader)
import           Data.Advent                     (Day (..))
import           Data.Bidim                      (Bidim, Coord, cross)
import           Data.Char                       (digitToInt)
import           Data.Functor.Identity           (Identity, runIdentity)
import           Data.Generics.Labels            ()
import qualified Data.HashMap.Lazy               as HasHSet
import           Data.HashSet                    (HashSet)
import qualified Data.HashSet                    as HashSet
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Maybe                      (fromJust)
import           Data.Text                       (intercalate)
import           System.IO.Advent                (getInput, getParsedInput,
                                                  solve)
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

-- TODO: This is to overocome a current inefficiency of Data.Bidim.boundaries
boundaries' :: (Coord, Coord)
boundaries' = ((0, 0), (99, 99))

astarConfig :: Bidim Int -> AstarConfig Int Node Coord (Bidim Int)
astarConfig = mkConfig h cost explode isGoal rememberNode seenNode

-- TODO This is incorrect and may be the reason why part two doesn't work, we
-- need to support adding nodes that reach a point that was already exploed, but
-- that the new node reaches with lower cost. At the moment we can only compare
-- with equality
rememberNode :: MonadReader (Bidim Int) m => HashSet Coord -> Node -> m (HashSet Coord)
rememberNode s n = pure $ HashSet.insert (view (#path . hd) n) s

seenNode :: MonadReader (Bidim Int) m => HashSet Coord -> Node -> m Bool
seenNode s n = pure . HashSet.member (view (#path . hd) n) $ s

-- TODO Figure out if there is a head lens for NonEmpty
hd :: Getter (NonEmpty a) a
hd = to NonEmpty.head

-- TODO add manhattan distance to bidim libraries
h :: MonadReader (Bidim Int) m => Node -> m Int
h node =
  let
    pos = views #path NonEmpty.head node
  in do
    -- (_, maxCoord) <- asks boundaries
    let (_, maxCoord) = boundaries'
    pure $ sumOf both maxCoord - sumOf both pos
{-# SCC h #-}

-- TODO add manhattan distance to bidim libraries
h2 :: MonadReader (Bidim Int) m => Node -> m Int
h2 node =
  let
    pos = views #path NonEmpty.head node
  in do
    -- (_, maxCoord) <- asks boundaries
    let (_, maxCoord) = boundaries2
    pure $ sumOf both maxCoord - sumOf both pos
{-# SCC h2 #-}

cost :: MonadReader (Bidim Int) m => Node -> m Int
cost = pure . view #cost

explode :: MonadReader (Bidim Int) m => Node -> m [Node]
explode node =
  do
    let limits = boundaries'
    let newPos =
          filter (inRange limits)
          . filter (not . flip HashSet.member (view #pathMem node))
          $ cross (view (#path . hd) node)
    traverse (addHop node) newPos
{-# SCC explode #-}

explode2 :: MonadReader (Bidim Int) m => Node -> m [Node]
explode2 node =
  do
    let limits = boundaries2
    let newPos =
          filter (inRange limits)
          . filter (not . flip HashSet.member (view #pathMem node))
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
      . over #pathMem (HashSet.insert pos)
      . over #cost (+ risk)
      $ node

getRisk :: MonadReader (Bidim Int) m => Coord -> m Int
getRisk pos = view (at pos . non 0)

addHop2 :: MonadReader (Bidim Int) m => Node -> Coord -> m Node
addHop2 node pos =
  do
    risk <- getRisk2 pos
    pure $
      over #path (pos `NonEmpty.cons`)
      . over #pathMem (HashSet.insert pos)
      . over #cost (+ risk)
      $ node

getRisk2 :: MonadReader (Bidim Int) m => Coord -> m Int
getRisk2 pos =
  let
    (basePos, addedRisk) = mapExtendedPos pos
  in do
    baseRisk <- getRisk basePos
    pure $ (baseRisk + addedRisk - 1) `mod` 10 + 1

-- Return the position in the base map and the added risk
mapExtendedPos :: Coord -> (Coord, Int)
mapExtendedPos pos =
  let
    maxX = view (_2 . _1) boundaries' + 1
    maxY = view (_2 . _2) boundaries' + 1
    addX = view _1 pos `div` maxX
    addY = view _2 pos `div` maxY
    basePos = over _1 (`mod` maxX)
              . over _2 (`mod` maxY)
              $ pos
  in
    (basePos, addX + addY)

isGoal :: MonadReader (Bidim Int) m => Node -> m Bool
isGoal node =
  let
    pos = view (#path . hd) node
  in
    -- asks (((pos ==) . view _2) . boundaries)
    pure (pos ==  view _2 boundaries')

initialNode :: Node
initialNode = Node ((0,0) :| []) (HashSet.singleton (0,0)) 0

astarConfig2 :: Bidim Int -> AstarConfig Int Node Coord (Bidim Int)
astarConfig2 = mkConfig h2 cost explode2 isGoal2 undefined undefined

boundaries2 :: (Coord, Coord)
boundaries2 = over (_2 . both) (subtract 1 . (*5) . (+ 1)) boundaries'

isGoal2 :: MonadReader (Bidim Int) m => Node -> m Bool
isGoal2 node =
  let
    pos = view (#path . hd) node
  in
    pure (pos == view _2 boundaries2)

-- TODO Add Astar and runAstar to Astar
-- TODO Maybe this can be faster with a Dijkstra MonadSearch
solver1 :: MonadEmit (Metrics Int Int) m => Parsed -> m Int
solver1 input = do
  (maybeNode, ()) <- searchAstarT (astarConfig input) initialNode
  pure . view (singular _Just . #cost) $ maybeNode

-- TODO This does not work, but I have not had time to verify that I am
-- extending the map faithfully. the ~toMem~ function is also incorrect
solver2 :: MonadEmit (Metrics Int Int) m => Parsed -> m Int
solver2 input = do
  (maybeNode, ()) <- searchAstarT (astarConfig2 input) initialNode
  pure . view (singular _Just . #cost) $ maybeNode

main :: IO ()
main = solveM day parser solver1 solver2
