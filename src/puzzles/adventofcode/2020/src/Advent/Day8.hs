{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day8 where

import           Advent.Perlude

import           Control.Lens             (at, both, each, foldlOf, ix,
                                           modifying, over, preuse, preview,
                                           use, uses, view, _1, _2, _head,
                                           _tail)
import           Control.Monad            (guard)
import           Control.Monad.Loops      (untilJust)
import           Control.Monad.State.Lazy (State, gets, modify)
import           Data.Generics.Labels     ()
import           Data.List                (find, foldl', sort, unfoldr)
import           Data.Map.Lazy            (Map, assocs, keysSet)
import qualified Data.Map.Lazy            as Map
import           Data.Maybe               (fromJust)
import           Data.Maybe               (isJust)
import           Data.Set                 (Set, difference, member)
import qualified Data.Set                 as Set
import           Data.Text                (Text, count, dropEnd, lines, pack,
                                           replace, singleton, splitOn,
                                           stripEnd, takeEnd, unpack)
import qualified Data.Text                as Text
import qualified System.IO.Advent         as IOAdvent
import qualified Text.Read                as Read

import           Advent.Day8.Internal

-- TODO: This is part of the most recent base (for String), make it for Text in
-- our prelude
readMaybe :: Read a => Text -> Maybe a
readMaybe = Read.readMaybe . unpack

example :: Text
example = "nop +0\n\
          \acc +1\n\
          \jmp +4\n\
          \acc +3\n\
          \jmp -3\n\
          \acc -99\n\
          \acc +1\n\
          \jmp -4\n\
          \acc +6"

getInput :: IO Text
getInput = IOAdvent.getInput "8"

parseInstruction :: Text -> Maybe Op
parseInstruction instruction =
  let
    -- TODO: there is probably an elegant way of doing this
    split = case Text.splitOn " " instruction
      of [a, b] -> Just (a, b)
         _      -> Nothing
  in do
    (opText, valueText) <- split
    value <- readMaybe . Text.dropWhile (== '+') $ valueText
    parseOp opText value

parseOp :: Text -> Int -> Maybe Op
parseOp "nop" = Just . Nop
parseOp "acc" = Just . Acc
parseOp "jmp" = Just . Jmp
parseOp _     = const Nothing

parse :: Text -> Maybe [Op]
parse = traverse parseInstruction . Text.lines

runOp :: Op -> VMState -> VMState
runOp (Nop _) = over #pc (+1)
runOp (Acc n) = over #pc (+1) . over #acc (+n)
runOp (Jmp n) = over #pc (+n)

step :: VMState -> Maybe VMState
step st =
  let pc = view #pc st
  in flip runOp st <$> preview (#code . ix pc) st

unfold :: VMState -> [VMState]
unfold st = case step st of
  Just next -> st : unfold next
  Nothing   -> [st]

findRepeated :: Set Int -> [VMState] -> Maybe VMState
findRepeated seen states = do
  current <- preview _head states
  left <- preview _tail states
  next <- preview _head left

  if Set.member (view #pc next) seen
    then Just current
    else findRepeated (Set.insert (view #pc current) seen) left

mutate :: Op -> Op
mutate (Nop x) = Jmp x
mutate (Jmp x) = Nop x
mutate x       = x

allMutations :: [Op] -> [[Op]]
allMutations []    = []
allMutations (h:t) = (mutate h : t) : ((h :) <$> allMutations t)

solution2 input =
  let
    mutations = fromJust $ allMutations <$> parse input
    repeated = findRepeated Set.empty . unfold . mkState <$> mutations
  in
    zip repeated (mkState <$> mutations)

main :: IO ()
main = do
  input <- getInput

  -- TODO: Clean this mess
  putStr "Solution 1: "
  print $ view #acc . fromJust . findRepeated Set.empty . unfold . mkState . fromJust . parse $ input

  -- TODO: UGH!
  putStr "Solution 2: "
  print $ view #acc . last . unfold $ view _2 . fromJust $ find ((== Nothing) . view _1) $ solution2 input
