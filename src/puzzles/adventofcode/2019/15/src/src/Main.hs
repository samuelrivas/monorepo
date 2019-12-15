{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

import           Prelude                hiding (Left, Right, concat, getLine,
                                         putStrLn, readFile, show)

import           Control.Lens           (assign, at, ix, modifying, non, over,
                                         set, set, toListOf, traverse, use,
                                         uses, view, view, _1, _2, _3)
import Control.Monad.Trans.Maybe (runMaybeT)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Loops    (whileM_)
import           Control.Monad.State    (StateT, get, lift, runStateT, when)
import           Data.Foldable          (maximum, minimum, traverse_)
import           Data.Functor.Identity  (runIdentity)
import           Data.Generics.Labels   ()
import           Data.Map.Strict        (Map, empty, fromList, keys, size)
import           Data.Text              (Text, concat, intercalate, splitOn,
                                         unpack)
import           Data.Text.IO           (putStrLn, readFile)
import           GHC.Generics           (Generic)
import           System.Console.ANSI    (clearScreen, setCursorPosition)
import Data.Sequence (Seq, (<|), (|>), (><))

import           Bidim
import           Intcode                hiding (initial_state)
import           Internal

type ExploreT m = StateT Exploration m

assert :: Bool -> ()
assert False = error "assertion failed"
assert True = ()

encode :: (Num c, Enum a) => a -> c
encode = fromIntegral . fromEnum

decode :: (Integral c, Enum a) => c -> a
decode = toEnum . fromIntegral

encodeMove :: Move -> Integer
encodeMove = (+1) . encode

allMoves :: [Move]
allMoves = [minBound..maxBound]

getInput :: IO [Integer]
getInput = fmap (read . unpack) . splitOn "," <$> readFile "input.txt"

popNode :: Monad m => ExploreT m (Maybe Node)
popNode = undefined

pushNode :: Monad m => Node -> ExploreT m ()
pushNode = undefined

-- Push only unexplored nodes to the queue
pushNodes :: Monad m => [Node] -> ExploreT m ()
pushNodes = undefined

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

explode :: IntcodeState -> Coord -> ExploreT m [Node]
explode = undefined

-- Returns the target node if found
-- Use maybeT here to remove noise
explore :: Monad m => ExploreT m ()
explore = popNode >>= \case
  Just node ->
    let
      pos :: Coord = view #pos node
    in
      use (#map . at pos) >>= \case
        Nothing -> do
          (cell, intcodeState) <- exploreNode node
          assign (#map . at pos) . Just $ cell
          explode intcodeState pos >>= pushNodes
        Just _ ->
          pure ()

  Nothing -> pure ()

exploreNode :: Monad m => Node -> ExploreT m (Cell, IntcodeState)
exploreNode node =
  let
    intcodeState = view #intcodeState node
    move = view #move node
    moveInstruction = reset intcodeState >> moveDroid move >> getOutput
  in do
    (intCell, intcodeState', _) <- lift $ runEmpty moveInstruction
    case decode <$> intCell of
      [cell] -> pure (cell, intcodeState')
      _ -> error "cannot decode!"

main :: IO ()
main = do
  code <- getInput
--  putStrLn $ "Solution 1: " <> show (size . fromList . parse_output . read_game $ code)
  undefined
