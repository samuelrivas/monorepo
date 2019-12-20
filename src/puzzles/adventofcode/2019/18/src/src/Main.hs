{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import           Prelude               hiding (lines, putStrLn, readFile, show,
                                        unlines)
import qualified Prelude

import           Control.Lens          (at, non, over, set, traverse, view,
                                        views, _1, _2)
import           Data.Char             (isAsciiLower, isAsciiUpper, toLower)
import           Data.Foldable         (find, foldl')
import           Data.Functor.Identity (runIdentity)
import           Data.Generics.Labels  ()
import           Data.List             (concatMap, unfoldr)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (fromMaybe, fromJust, catMaybes)
import           Data.Ratio            ((%))
import           Data.Text             (Text, intercalate, lines, pack, splitOn,
                                        unpack)
import qualified Data.Text             as Text
import           Data.Text.IO          (putStrLn, readFile)
import Control.Monad.Reader (Reader)
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashSet as HashSet

import           Astar
import           Bidim
import           Internal
import MonadSearch (step, search)

show :: Show a => a -> Text
show = pack . Prelude.show

solve1 :: Text -> IO ()
solve1 text =
  let
    (maze, starting, keys) = parseInput text
  in do
    (node, _ctx, trace) <- runInAstarT
                          search
                          (astarConfig maze)
                          (initialNode starting (length keys))
    putStrLn $ "Done: " <> show node
    putStrLn $ "Sol: " <> show (views #path length (fromJust node))
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

solve2 :: a -> IO ()
solve2 = undefined

getInput :: IO Text
getInput = readFile "input.txt"

parseInput :: Text -> (Bidim Char, Coord, [Char])
parseInput text =
  let bidim = fromText text
  in (bidim, getStartingPoint bidim, getKeys bidim)

getStartingPoint :: Bidim Char -> Coord
getStartingPoint bidim =
  let tupleM = find (views _2 (== '@')) $ Map.toList bidim
  in view _1 . fromJust $ tupleM

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
    startingNode = initialNode startingPoint (length keys)
  in
    runIdentity $ searchAstarT config startingNode

heuristic :: MazeNode -> Reader MazeContext Int
heuristic = pure . view #h

cost :: MazeNode -> Reader MazeContext Int
cost = pure . view #c

explode :: MazeNode -> Reader MazeContext [MazeNode]
explode node =
  let candidates = cross (view #pos node)
  in catMaybes <$> mapM (candidateToNode node) candidates

nodeToMem :: MazeNode -> Reader MazeContext MazeMemory
nodeToMem = pure . toMemory

gotAllKeys :: MazeNode -> Reader MazeContext Bool
gotAllKeys = fmap (== 0) . heuristic

candidateToNode :: MazeNode -> Coord -> Reader MazeContext (Maybe MazeNode)
candidateToNode fromNode toPos =
  view (at toPos) >>= \case
    Nothing -> pure Nothing
    Just '#' -> pure Nothing
    Just '.' -> pure . Just $ nextNode fromNode toPos
    Just '@' -> pure . Just $ nextNode fromNode toPos
    Just letter
      | isAsciiLower letter ->
        nodeForKey fromNode toPos letter
      | isAsciiUpper letter ->
        nodeForDoor fromNode toPos letter
      | otherwise -> error $ "found " <>  [letter]

nodeForKey :: MazeNode -> Coord -> Char -> Reader MazeContext (Maybe MazeNode)
nodeForKey parent toPos key =
  let
    keys = view #keys parent
    baseNewNode = nextNode parent toPos
  in
    if HashSet.member key keys
    then pure . Just $ baseNewNode
    else pure . Just $
           over #h (+ (- hValueOfKey)) $
           over #keys (HashSet.insert key) baseNewNode

nodeForDoor :: MazeNode -> Coord -> Char -> Reader MazeContext (Maybe MazeNode)
nodeForDoor parent toPos door =
  let
    keys = view #keys parent
    key = toLower door
  in
    if HashSet.member key keys
    then pure . Just $ nextNode parent toPos
    else pure Nothing

nextNode :: MazeNode -> Coord -> MazeNode
nextNode fromNode toPos =
  let
    fromPos = view #pos fromNode
  in
    set #pos toPos $
    over #path (fromPos :) $
    over #c (+ 1)
    fromNode

main :: IO ()
main = do
  input <- getInput
  solve1 input
  solve2 input
