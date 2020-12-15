{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day14 where

import           Advent.Perlude

import           Control.Lens          (at, both, each, foldlOf, over, set,
                                        sets, view, _2)
import           Control.Monad         (guard)
import           Data.Bits             (clearBit, setBit)
import           Data.Char             (isDigit)
import           Data.Generics.Labels  ()
import           Data.List             (elemIndices, find, foldl', sort,
                                        unfoldr)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (fromJust, isJust)
import           Data.Set              (Set)
import qualified Data.Set              as Set
import qualified Data.Text             as Text
import qualified System.IO.Advent      as IOAdvent

import           Advent.Day14.Internal

example :: Text
example = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
          \mem[8] = 11\n\
          \mem[7] = 101\n\
          \mem[8] = 0\n"

getInput :: IO Text
getInput = IOAdvent.getInput "14"

parse :: Text -> [Instruction]
parse = fmap (parseInstruction <$> Text.splitOn " = ") . Text.lines

parseInstruction :: [Text] -> Instruction
parseInstruction ["mask", mask] = Mask mask
parseInstruction [mem, value] =
  let
    pos = read . Text.dropAround (not . isDigit) $ mem
    val = read value
  in
    Mem pos val
parseInstruction _ = undefined

step :: ComputerState -> Instruction -> ComputerState
step st (Mask mask)      = set #mask mask st
step st (Mem addr value) =
  let
    mask = view #mask st
    newValue = applyMask mask value
  in
    over #memory (Map.insert addr newValue) st

applyMask :: Text -> Int -> Int
applyMask mask value =
  let
    f char (pos, num) =
      case char of
        'X' -> (pos + 1, num)
        '1' -> (pos + 1, num `setBit` pos)
        '0' -> (pos + 1, num `clearBit` pos)
        _   -> undefined
  in
    view _2 $ Text.foldr f (0, value) mask

run :: ComputerState -> [Instruction] -> ComputerState
run = foldl' step

solve :: [Instruction] -> Int
solve = Map.foldl' (+) 0 . view #memory . run mkComputerState

main :: IO ()
main = do
  input <- parse <$> getInput

  putStr "Solution 1: "
  print $ solve input

  putStr "Solution 2: "
  print $ "NA"
