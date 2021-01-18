{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Advent.Day8 where

import           Advent.Perlude

import           Advent.Templib.Parsec (digitsAsNum, literal)
import           Control.Lens          (ix, over, preview, view, _1, _2, _head,
                                        _tail)
import           Data.Functor          (($>))
import           Data.Generics.Labels  ()
import           Data.List             (find)
import           Data.Maybe            (fromJust)
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Text.Parsec           (char, sepEndBy, (<|>))
import           Text.Parsec.Text      (Parser)


import           Advent.Templib        (Day (..), getInput', getParsedInput)

import           Advent.Day8.Internal

day :: Day
day = D8

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
getInput = getInput' day

parseOp :: Parser (Int -> Op)
parseOp =
  literal "nop" $> Nop
  <|> literal "acc" $> Acc
  <|> literal "jmp" $> Jmp

parseArg :: Parser Int
parseArg = do
  sign <- char '-' $> -1 <|> char '+' $> 1
  (* sign) <$> digitsAsNum

parseInstruction :: Parser Op
parseInstruction =
  parseOp <* literal " "
  <*> parseArg

parser :: Parser [Op]
parser = parseInstruction `sepEndBy` char '\n'

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

solution2 :: [Op] -> [(Maybe VMState, VMState)]
solution2 input =
  let
    mutations = allMutations input
    repeated = findRepeated Set.empty . unfold . mkState <$> mutations
  in
    zip repeated (mkState <$> mutations)

main :: IO ()
main = do
  instructions <- getParsedInput day parser

  -- TODO: Clean this mess
  putStr "Solution 1: "
  print . view #acc . fromJust . findRepeated Set.empty . unfold . mkState $ instructions

  -- TODO: UGH!
  putStr "Solution 2: "
  print . view #acc . last . unfold $ view _2 . fromJust $ find ((== Nothing) . view _1) $ solution2 instructions
