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
import           Advent.Templib                  (linesOf)

import           Control.Lens                    (Getter, _1, _2, _Just, at,
                                                  both, non, over, preview,
                                                  singular, sumOf, to, use,
                                                  view, views)
import           Control.Monad                   (replicateM_)
import           Control.Monad.MonadSearch.Astar (AstarConfig, mkConfig,
                                                  searchAstarT)
import           Control.Monad.Reader            (MonadReader, ReaderT, asks,
                                                  runReaderT)
import           Control.Monad.State             (MonadState, State, gets,
                                                  modify, runState)
import           Data.Advent                     (Day (..))
import           Data.Bidim                      (Bidim, Coord, boundaries,
                                                  cross)
import           Data.Char                       (digitToInt)
import           Data.Functor.Identity           (Identity, runIdentity)
import           Data.Generics.Labels            ()
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as HashMap
import           Data.HashSet                    (HashSet)
import qualified Data.HashSet                    as HashSet
import           Data.Hashable                   (hash)
import           Data.List                       (sortOn)
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Maybe                      (catMaybes, fromJust)
import           Data.MultiSet                   (MultiSet)
import qualified Data.MultiSet                   as MultiSet
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Text                       (intercalate)
import qualified Data.Text                       as Text
import           System.IO.Advent                (getInput, getParsedInput,
                                                  solve)
import           Text.Parsec                     (anyChar, noneOf)
import           Text.Parsec.Bidim               (bidim)
import           Text.Parsec.Parselib            (Parser, literal, text1,
                                                  unsafeParseAll)

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

astarConfig :: Bidim Int -> AstarConfig Node Coord (Bidim Int)
astarConfig = mkConfig h cost explode isGoal toMem

-- TODO This is incorrect and may be the reason why part two doesn't work, we
-- need to support adding nodes that reach a point that was already exploed, but
-- that the new node reaches with lower cost. At the moment we can only compare
-- with equality
toMem :: MonadReader (Bidim Int) m => Node -> m Coord
toMem = pure . view (#path . hd)

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

astarConfig2 :: Bidim Int -> AstarConfig Node Coord (Bidim Int)
astarConfig2 = mkConfig h cost explode2 isGoal2 toMem

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
solver1 :: Parsed -> Int
solver1 input =
  view (_1 . singular _Just . #cost) . runIdentity
  $ (searchAstarT (astarConfig input) initialNode :: Identity (Maybe Node, ()))

-- TODO This does not work, but I have not had time to verify that I am
-- extending the map faithfully. the ~toMem~ function is also incorrect
solver2 :: Parsed -> Int
solver2 input =
  view (_1 . singular _Just . #cost) . runIdentity
  $ (searchAstarT (astarConfig2 input) initialNode :: Identity (Maybe Node, ()))


main :: IO ()
main = solve day parser solver1 solver2
