-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

-- Ideas to speed this up
--
--  - Graph compression: Generate a graph linking doors, keys and crossroads,
--    compressing all pahts into a single transition with cost equal to the
--    length of the path. This might be quite some work
--
--  - Generate better heuristics. For example: a function of the Manhattan
--    distances to all the doors, where the necessary keys are taking into
--    account. The distance to a door for which we don't have the key should be
--    higher than any possible distance so that we force the algorithm to move
--    towards the necessary keys. Blocked keys may add the cost of walking
--    towards the door. This seems more promising to me, but I don't know how to
--    easily check whether a key is blocked...

module Advent.Day18 where

import           Prelude                         hiding (lines, putStrLn, show,
                                                  unlines)
import qualified Prelude

import           Control.Lens                    (at, ix, over, preview, set,
                                                  view, views, _1, _2)
import           Control.Monad                   (replicateM_)
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.MonadSearch       (step)
import           Control.Monad.MonadSearch.Astar
import           Control.Monad.Reader            (Reader)
import           Data.Bidim                      (Bidim, Coord, cross, fromText,
                                                  plus)
import           Data.Char                       (isAsciiLower, isAsciiUpper,
                                                  toLower)
import           Data.Foldable                   (find)
import           Data.Functor.Identity           (runIdentity)
import           Data.Generics.Labels            ()
import qualified Data.HashSet                    as HashSet
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (catMaybes, fromJust)
import           Data.Text                       (Text, pack)
import           Data.Text.IO                    (putStrLn)
import qualified System.IO.Advent                as Advent

import           Advent.Day18.Internal

show :: Show a => a -> Text
show = pack . Prelude.show

solve1 :: Text -> IO ()
solve1 text =
  let
    (maze, starting, keys) = parseInput text
  in do
    (node, _trace :: ()) <- searchAstarT
                            (astarConfig maze)
                            (initialNode [starting] (length keys))
    -- putStrLn $ "Done: " <> show node
    putStrLn $ "Solution 1: " <> show (views #path length (fromJust node))
--    putStrLn trace

watchSearch :: Int -> AstarT MazeNode MazeMemory (Bidim Char) Text IO ()
watchSearch steps = do
  replicateM_ steps step
  peekBest >>= \case
    Just node ->
      if view #h node == 0
      then
        liftIO $ putStrLn $ "Found! " <>
          show node <> "\n" <>
          show (views #path length node)
      else do

        liftIO . putStrLn $ show (view #pos node) <>
          " c: " <> show (view #c node) <>
          " h: " <> show (view #h node) <>
          " k: " <> show (view #keys node)
        watchSearch steps
    Nothing -> liftIO $ putStrLn "Done"

solve2 :: Text -> IO ()
solve2 text =
  let
    (maze, starting, keys) = parseInput2 text
  in do
    (node, _trace :: ()) <- searchAstarT
                            (astarConfig maze)
                            (initialNode starting (length keys))
    -- putStrLn $ "Done: " <> show node
--    putStrLn $  showBidim (Text.singleton . fromJust) maze
    putStrLn $ "Solution 2: " <> show (views #path length (fromJust node))
    -- putStrLn $ show starting
    -- putStrLn "----------"
    -- putStrLn $ Text.unlines . reverse $ (show <$> view #path (fromJust node))

getInput :: IO Text
getInput = Advent.getInput "18"

parseInput :: Text -> (Bidim Char, Coord, [Char])
parseInput text =
  let bidim = fromText text
  in (bidim, getStartingPoint bidim, getKeys bidim)

parseInput2 :: Text -> (Bidim Char, [Coord], [Char])
parseInput2 text =
  let
    (bidim, center, keys) = parseInput text
    wallCoords = center : cross center
    newWall = Map.fromList $ (, '#') <$> wallCoords
    newBidim = Map.union newWall bidim
  in
    (newBidim, getStartingPoints center, keys)

getStartingPoint :: Bidim Char -> Coord
getStartingPoint bidim =
  let tupleM = find (views _2 (== '@')) $ Map.toList bidim
  in view _1 . fromJust $ tupleM

getStartingPoints :: Coord -> [Coord]
getStartingPoints center =
  (center `plus`) <$> [(1, 1), (1, -1), (-1, 1), (-1, -1)]

getKeys :: Bidim Char -> [Char]
getKeys = filter isAsciiLower . Map.elems

astarConfig :: Bidim Char -> AstarConfig MazeNode MazeMemory MazeContext
astarConfig = mkConfig heuristic cost explode gotAllKeys nodeToMem

type MazeT m a = AstarT MazeNode (Bidim Char) Text m a

findBest :: Bidim Char -> (Maybe MazeNode, Text)
findBest maze =
  let
    startingPoint = getStartingPoint maze
    keys = getKeys maze
    config = mkConfig heuristic cost explode gotAllKeys nodeToMem maze
    startingNode = initialNode [startingPoint] (length keys)
  in
    runIdentity $ searchAstarT config startingNode

heuristic :: MazeNode -> Reader MazeContext Int
heuristic = pure . view #h

cost :: MazeNode -> Reader MazeContext Int
cost = pure . view #c

explode :: MazeNode -> Reader MazeContext [MazeNode]
explode node =
  let
    robots = [0..(length . view #pos $ node) - 1]
  in concat <$> sequence (explodeRobot node <$> robots)

explodeRobot :: MazeNode -> Int -> Reader MazeContext [MazeNode]
explodeRobot node robotIx =
  let
    candidates = cross . fromJust $ preview (#pos . ix robotIx) node
  in catMaybes <$> mapM (candidateToNode node robotIx) candidates

nodeToMem :: MazeNode -> Reader MazeContext MazeMemory
nodeToMem = pure . toMemory

gotAllKeys :: MazeNode -> Reader MazeContext Bool
gotAllKeys = fmap (== 0) . heuristic

candidateToNode :: MazeNode -> Int -> Coord -> Reader MazeContext (Maybe MazeNode)
candidateToNode fromNode robotIx toPos =
  view (at toPos) >>= \case
    Nothing -> pure Nothing
    Just '#' -> pure Nothing
    Just '.' -> pure . Just $ nextNode fromNode robotIx toPos
    Just '@' -> pure . Just $ nextNode fromNode robotIx toPos
    Just letter
      | isAsciiLower letter ->
        nodeForKey fromNode robotIx toPos letter
      | isAsciiUpper letter ->
        nodeForDoor fromNode robotIx toPos letter
      | otherwise -> error $ "found " <>  [letter]

nodeForKey :: MazeNode -> Int -> Coord -> Char -> Reader MazeContext (Maybe MazeNode)
nodeForKey parent robotIx toPos key =
  let
    keys = view #keys parent
    baseNewNode = nextNode parent robotIx toPos
  in
    if HashSet.member key keys
    then pure . Just $ baseNewNode
    else pure . Just $
           over #h (+ (- hValueOfKey)) $
           over #keys (HashSet.insert key) baseNewNode

nodeForDoor :: MazeNode -> Int -> Coord -> Char -> Reader MazeContext (Maybe MazeNode)
nodeForDoor parent robotIx toPos door =
  let
    keys = view #keys parent
    key = toLower door
  in
    if HashSet.member key keys
    then pure . Just $ nextNode parent robotIx toPos
    else pure Nothing

nextNode :: MazeNode -> Int -> Coord -> MazeNode
nextNode fromNode robotIx toPos =
  let
    fromPos = view #pos fromNode
  in
    set (#pos . ix robotIx) toPos $
    set #robotIx robotIx $
    over #path (fromPos :) $
    over #c (+ 1)
    fromNode

main :: IO ()
main = do
  input <- getInput
  solve1 input
  solve2 input
