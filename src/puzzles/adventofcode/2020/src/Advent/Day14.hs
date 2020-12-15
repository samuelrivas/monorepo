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

-- TODO: this feels convoluted, there must be a better monadic approach
--
-- In any case, this is using the Monad instance for list, which is used to
-- represent non-determinism. The interpretation of binding a function is that
-- the function returns a list of possible results, which is exactly of what we
-- want
applyAddrMask :: Text -> Int -> [Int]
applyAddrMask mask value =
  let
    f char (pos, addrs) = (pos + 1, addrs >>= applyAddrMaskBit char pos)
  in
    view _2 $ Text.foldr f (0, [value]) mask

step2 :: ComputerState -> Instruction -> ComputerState
step2 st (Mask mask)      = set #mask mask st
step2 st (Mem addr value) =
  let
    mask = view #mask st
    addrs = applyAddrMask mask addr
    newMemory = foldl' (\memory addr' -> Map.insert addr' value memory) (view #memory st) addrs  in
    set #memory newMemory st


-- TODO: use Data.Bits instead of Ints for this
applyAddrMaskBit :: Char -> Int -> Int -> [Int]
applyAddrMaskBit '1' pos value  = [value `setBit` pos]
applyAddrMaskBit '0' _pos value = [value]
applyAddrMaskBit 'X' pos value  = [value `setBit` pos, value `clearBit` pos]
applyAddrMaskBit _ _ _          = undefined

run :: ComputerState -> [Instruction] -> ComputerState
run = foldl' step

run2 :: ComputerState -> [Instruction] -> ComputerState
run2 = foldl' step2

solve :: [Instruction] -> Int
solve = Map.foldl' (+) 0 . view #memory . run mkComputerState

solve2 :: [Instruction] -> Int
solve2 = Map.foldl' (+) 0 . view #memory . run2 mkComputerState

main :: IO ()
main = do
  input <- parse <$> getInput

  putStr "Solution 1: "
  print $ solve input

  putStr "Solution 2: "
  print $ solve2 input
