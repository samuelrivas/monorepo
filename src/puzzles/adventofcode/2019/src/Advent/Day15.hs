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
{-# LANGUAGE ScopedTypeVariables   #-}

module Advent.Day15 where

import           Prelude               hiding (Left, Right, concat, getLine,
                                        putStrLn, readFile, show)
import qualified Prelude

import           Advent.Day15.Intcode
import           Advent.Day15.Internal
import           Control.Lens          (assign, at, modifying, set, use, view)
import           Control.Monad         (when)
import           Control.Monad.Loops   (untilJust)
import           Control.Monad.RWS.CPS (RWST, evalRWST, execRWST, lift)
import           Data.Advent           (Day (..))
import           Data.Bidim            (Coord, showBidim)
import           Data.Functor.Identity (runIdentity)
import           Data.Generics.Labels  ()
import           Data.Map.Strict       (Map, empty)
import           Data.Maybe            (isNothing)
import           Data.Sequence         (Seq ((:<|)), fromList, (><), (|>))
import qualified Data.Sequence         as Seq
import           Data.Text             (Text, pack, splitOn, unpack)
import           Data.Text.IO          (putStrLn)
import           System.IO.Advent      (getInput)

show :: Show a => a -> Text
show = pack . Prelude.show

type ExploreT m = RWST () Text Exploration m

-- Uncomment to enable tracing
trsh :: Monad m => [Text] -> ExploreT m ()
-- trsh = tell . fold . (<> ["\n"])
trsh _ = pure ()

assert :: Bool -> ()
assert False = error "assertion failed"
assert True  = ()

encode :: (Num c, Enum a) => a -> c
encode = fromIntegral . fromEnum

decode :: (Integral c, Enum a) => c -> a
decode = toEnum . fromIntegral

encodeMove :: Move -> Integer
encodeMove = (+1) . encode

getIntcode :: IO [Integer]
getIntcode = fmap (read . unpack) . splitOn "," <$> getInput D15

popNode :: Monad m => ExploreT m (Maybe Node)
popNode =
  use #nodes >>= \case
    x :<| t -> do
      assign #nodes t
      pure $ Just x
    _ ->
      pure Nothing

-- Fixme, remove duplication with explodeNode
pushInitialNodes :: Monad m => [Integer] -> ExploreT m ()
pushInitialNodes code =
  let
    intcodeState = initialState code
  in do
    pushNode $ mkNode North (0, 1) intcodeState [North]
    pushNode $ mkNode South (0, -1) intcodeState [South]
    pushNode $ mkNode West (-1, 0) intcodeState [West]
    pushNode $ mkNode East (1, 0) intcodeState [East]

pushNode :: Monad m => Node -> ExploreT m ()
pushNode = modifying #nodes . flip (|>)

-- Push only unexplored nodes to the queue
pushNodes :: Monad m => [Node] -> ExploreT m ()
pushNodes nodes =
  do
    cellMap <- use #map
    let p node = isNothing $ view (at (view #pos node)) cellMap
        newNodes = fromList . filter p $ nodes

    modifying #nodes (>< newNodes)

moveDroid :: Monad m => Move -> IntcodeT m Cell
moveDroid move = do
  assert . (== Interrupted) <$> getStatus
  pushInput [encodeMove move]
  runProgram
  getOutput >>= \case
    [output] -> do
      flushOutput
      pure $ decode output
    _ -> error "too many outputs"

-- Return neighbour nodes from a given node
explodeNode :: Node -> IntcodeState -> [Node]
explodeNode node intcodeState =
  let
    (x, y) = view #pos node
    path = view #path node
  in
    [mkNode North (x, y + 1) intcodeState (North : path),
     mkNode South (x, y - 1) intcodeState (South : path),
     mkNode West (x - 1, y) intcodeState (West : path),
     mkNode East (x + 1, y) intcodeState (East : path)]

-- Returns True if the target was hit, False if it hasn't and Nothing if more
-- steps are needed. Running this consecutively eventually yields a result,
-- unless the labyrinth is infinite
explore :: Monad m => ExploreT m (Maybe Bool)
explore =
  popNode >>= \case
    Nothing -> do
      trsh ["no more nodes, stopping"]
      pure $ Just False
    Just node ->
      let pos = view #pos node
          path = view #path node
      in use (#map . at pos) >>= \case
        Nothing -> do
          (cell, nextIntcodeState) <- evalNode node
          assign (#map . at pos) . Just $ cell
          trsh ["pos ", show pos, " is ", show cell]
          when (cell == Empty) $ do
            trsh ["Exploding ", show pos]
            pushNodes $ explodeNode node nextIntcodeState
          if cell == Goal
            then do
            trsh ["Found Goal at ", show pos]
            assign #goal $ Just (pos, path)
            assign #goalNode $ Just node
            pure $ Just True
          else
            pure Nothing
        Just _ -> do
          trsh ["Revisiting ", show pos, " doing nothing"]
          pure Nothing

-- Fix this copypaste!
expand :: Monad m => ExploreT m (Maybe Integer)
expand =
  popNode >>= \case
    Nothing -> do
      trsh ["no more nodes, stopping"]
      time <- use #maxPath
      pure $ Just time
    Just node ->
      let pos = view #pos node
          path = view #path node
      in use (#map . at pos) >>= \case
        Nothing -> do
          (cell, nextIntcodeState) <- evalNode node
          assign (#map . at pos) . Just $ cell
          trsh ["pos ", show pos, " is ", show cell]
          when (cell == Empty || cell == Goal) $ do
            trsh ["Exploding ", show pos]
            pushNodes $ explodeNode node nextIntcodeState
            modifying #maxPath (max (fromIntegral . length $ path))
          pure Nothing
        Just _ -> do
          trsh ["Revisiting ", show pos, " doing nothing"]
          pure Nothing

evalNode :: Monad m => Node -> ExploreT m (Cell, IntcodeState)
evalNode node =
  let
    intcodeState = view #intcodeState node
    move = view #move node
    moveInstruction = reset intcodeState >> moveDroid move
  in do
    (cell, intcodeState', _) <- lift $ runEmpty moveInstruction
    pure (cell, intcodeState')

search ::
  Monad m => [Integer] -> ExploreT m (Map Coord Cell, Maybe (Coord, [Move]))
search intcode = do
  pushInitialNodes intcode
  _ <- untilJust explore
  -- _ <- replicateM_ 30 explore
  (,) <$> use #map <*> use #goal

solution_1 :: [Integer] -> ((Map Coord Cell, Maybe (Coord, [Move])), Text)
solution_1 intcode = runIdentity . evalRWST (search intcode) () $ mkExploration

formatCell :: Maybe Cell -> Text
formatCell Nothing      = " "
formatCell (Just Wall)  = "#"
formatCell (Just Empty) = "."
formatCell (Just Goal)  = "X"

search_2 ::
  Monad m => Node -> ExploreT m (Map Coord Cell, Integer)
search_2 startNode = do
  pushNode startNode
  _ <- untilJust expand
  -- _ <- replicateM_ 30 expand
  (,) <$> use #map <*> use #maxPath

-- This is horrible, but works for now. Find the oxygen system using the first
-- part, then reset the state erasing the map and the remaining search
-- nodes. After that restart the search from the oxygen position, but using the
-- slightly modified search_2 which will exhaust the search and keep track of
-- the longest path in the state
solution_2 :: [Integer] -> ((Map Coord Cell, Integer), Text)
solution_2 intcode =
  let
    (departingState, _) = runIdentity . execRWST (search intcode) () $ mkExploration
    Just departingNode = view #goalNode departingState
    newNode = set #path [] departingNode
    newState = set #map empty $ set #nodes Seq.empty  departingState
  in
    runIdentity . evalRWST (search_2 newNode) () $ newState

main :: IO ()
main = do
  code <- getIntcode
  let ((cellMap, Just (coord, path)), searchLog) = solution_1 code

  putStrLn $ "Solution 1: " <> (show . length  $ path)
  putStrLn $ "At " <> show coord
  putStrLn $ showBidim formatCell cellMap
  putStrLn searchLog

  let ((cellMap', time), searchLog') = solution_2 code
  putStrLn $ "Solution 2: " <> show time
  putStrLn $ showBidim formatCell cellMap'
  putStrLn searchLog'
