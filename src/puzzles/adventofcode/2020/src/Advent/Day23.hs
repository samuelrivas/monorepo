{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day23 where

import           Perlude

import           Control.Lens          (assign, at, both, each, foldlOf, non,
                                        over, use, view, _1, _2)
import           Control.Monad         (guard)
import           Control.Monad.Loops
import           Control.Monad.State
import           Data.Generics.Labels  ()
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HashMap
import           Data.List             (find, foldl', sort, unfoldr)
import           Data.Maybe            (fromJust, isJust)
import           Data.Set              (Set)
import qualified Data.Set              as Set
import qualified Data.Text             as Text
import qualified System.IO.Advent      as IOAdvent

import           Advent.Day23.Internal

-- TODO: Move to utils
modAbs :: Int -> Int -> Int
modAbs modulus n =
    case n `rem` modulus of
      x | x < 0 -> modulus + x
        | otherwise -> x

example :: Text
example = "32415"

getInput :: IO Text
getInput = IOAdvent.getInput "23"

step :: MonadState Game m => m (Int, Int)
step = do
  pos <- use #pos
  currentValue <- fromJust <$> use (#cups . at pos)
  dest <- destinationCup currentValue

  -- now move all values 3 positions to the left, towards current pos

  pure (currentValue, dest)

destinationCup :: MonadState Game m => Int -> m Int
destinationCup currentValue = do
  pos <- use #pos
  modulus <- HashMap.size <$> use #cups
  let
    modulise = modAbs modulus
    candidates = modulise <$> reverse [(currentValue - 4) .. (currentValue - 1)]
    out = modulise <$> [(pos + 1) .. (pos + 4)]

  pure . head . dropWhile (`elem` out) $ candidates

-- Fill in the positions right of pos until we find the destination value
shift :: MonadState Game m => Int -> m ()
shift destinationValue = do
  pos <- use #pos
  m <- modulise

  -- Make a hole starting at destination value
  destinationPos <- shiftValue destinationValue (m $ pos + 1)

  -- TODO: undo all this copy pasta
  valueToMove1 <- use $ #cups . at (pos + 1)
  valueToMove2 <- use $ #cups . at (pos + 2)
  valueToMove3 <- use $ #cups . at (pos + 3)

  let
  assign (#cups . at destinationPos) valueToMove1
  assign (#cups . at (m $ destinationPos + 1)) valueToMove2
  assign (#cups . at (m $ destinationPos + 2)) valueToMove3

-- After skipping the values in pos, pos + 1 and pos +3, moves consequtive to
-- pos until it finds destination value, moving it and returning the first free
-- position (the one where value was
--
-- Returns true when finished shifting
shiftValue :: MonadState Game m => Int -> Int -> m Int
shiftValue destinationValue destinationPos = do
  m <- modulise
  let
    orig = m $ destinationPos + 3
  movedValue <- fromJust <$> use (#cups . at orig)
  assign (#cups . at destinationPos) $ Just movedValue
  if movedValue == destinationValue
    then pure destinationPos
    else shiftValue destinationValue (m $ destinationPos + 1)

modulise :: MonadState Game m => m (Int -> Int)
modulise = modAbs . HashMap.size <$> use #cups

main :: IO ()
main = do
  input <- getInput

  putStr "Solution 1: "
  print $ "NA"

  putStr "Solution 2: "
  print $ "NA"
