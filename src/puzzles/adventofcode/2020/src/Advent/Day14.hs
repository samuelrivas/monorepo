{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Advent.Day14 where

import           Advent.Perlude

import           Control.Lens          (over, set, view, _2)
import           Data.Bits             (clearBit, setBit)
import           Data.Generics.Labels  ()
import           Data.List             (foldl')
import qualified Data.Map              as Map
import qualified Data.Text             as Text
import           Text.Parsec           (between, char, oneOf, sepEndBy, try,
                                        (<?>), (<|>))

import           Advent.Day14.Internal
import           Advent.Templib        (Day (..), getInput', getParsedInput)
import           Advent.Templib.Parsec (Parser, literal, num, text1)

day :: Day
day = D14

example :: Text
example = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
          \mem[8] = 11\n\
          \mem[7] = 101\n\
          \mem[8] = 0\n"

getInput :: IO Text
getInput = getInput' D14

parser :: Parser [Instruction]
parser = parseInstruction `sepEndBy` char '\n'

parseInstruction :: Parser Instruction
parseInstruction  = try parseMask <|> try parseMem <?> "expression"

parseMask :: Parser Instruction
parseMask = Mask <$> (literal "mask = " *> text1 (oneOf "X01"))

parseMem :: Parser Instruction
parseMem =
  Mem
  <$> between (literal "mem[") (literal "]") num <* literal " = "
  <*> num

step :: ComputerState -> Instruction -> ComputerState
step st (Mask mask)      = set #mask mask st
step st (Mem addr value) =
  let
    mask = view #mask st
    newValue = applyMask mask value

  in over #memory (Map.insert addr newValue) st

applyMask :: Text -> Int -> Int
applyMask mask value =
  let
    f c (pos, n) =
      case c of
        'X' -> (pos + 1, n)
        '1' -> (pos + 1, n `setBit` pos)
        '0' -> (pos + 1, n `clearBit` pos)
        _   -> undefined

  in view _2 $ Text.foldr f (0, value) mask

-- TODO: this feels convoluted, there must be a better monadic approach
--
-- In any case, this is using the Monad instance for list, which is used to
-- represent non-determinism. The interpretation of binding a function is that
-- the function returns a list of possible results, which is exactly of what we
-- want
applyAddrMask :: Text -> Int -> [Int]
applyAddrMask mask value =
  let f c (pos, addrs) = (pos + 1, addrs >>= applyAddrMaskBit c pos)
  in view _2 $ Text.foldr f (0, [value]) mask

step2 :: ComputerState -> Instruction -> ComputerState
step2 st (Mask mask)      = step st (Mask mask)
step2 st (Mem addr value) =
  let
    mask = view #mask st
    addrs = applyAddrMask mask addr
    newMemory = foldl'
      (\memory addr' -> Map.insert addr' value memory)
      (view #memory st)
      addrs

  in set #memory newMemory st

-- TODO: We can easily parse this into a list of a better type to avoid the
-- undefined clause at the end
applyAddrMaskBit :: Char -> Int -> Int -> [Int]
applyAddrMaskBit '1' pos value  = [value `setBit` pos]
applyAddrMaskBit '0' _pos value = [value]
applyAddrMaskBit 'X' pos value  = [value `setBit` pos, value `clearBit` pos]
applyAddrMaskBit _ _ _          = undefined

solve :: (ComputerState -> Instruction -> ComputerState) -> [Instruction] -> Int
solve stepper = Map.foldl' (+) 0 . view #memory . foldl' stepper mkComputerState

main :: IO ()
main = do
  input <- getParsedInput day parser

  putStr "Solution 1: "
  print $ solve step input

  putStr "Solution 2: "
  print $ solve step2 input
