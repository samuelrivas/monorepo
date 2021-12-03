{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day2 where

import           Perlude

import           Control.Lens         (_1, _2, both, over, productOf)
import           Control.Monad        (foldM)
import           Control.Monad.State  (MonadState, evalState, get, modify)
import           Data.Advent          (Day (..))
import           Data.Bidim           (Coord)
import           Data.Foldable        (foldl')
import           Data.Functor         (($>))
import           System.IO.Advent     (getInput, solve)
import           Text.Parsec          ((<|>))
import           Text.Parsec.Parselib (Parser, digitsAsNum, literal)

import           Advent.Templib       (linesOf)

data Instruction =
  Forward Int
  | Up Int
  | Down Int
  deriving stock (Show, Eq)

day :: Day
day = D2

rawInput :: IO Text
rawInput = getInput day

parser :: Parser [Instruction]
parser = linesOf instruction

instruction :: Parser Instruction
instruction =
  do
    direction <- literal "forward " $> Forward
                 <|> literal "down " $> Down
                 <|> literal "up " $> Up
    direction <$> digitsAsNum

updatePos :: Instruction -> Coord -> Coord
updatePos (Forward n) = over _1 (+n)
updatePos (Up n)      = over _2 (subtract n)
updatePos (Down n)    = over _2 (+n)

updatePos2 :: MonadState Int m => Instruction -> Coord -> m Coord
-- updatePos' (Forward n) = over _1 (+n)
updatePos2 (Up n)      = (modify (subtract n) $>)
updatePos2 (Down n)    = (modify (+ n) $>)
updatePos2 (Forward n) = \coord -> do
  aim <- get
  pure $ over _1 (+n) . over _2 (+ n*aim) $ coord

-- TODO I am pretty sure there is a prettier lens operation lurking around here.
solver1 :: [Instruction] -> Int
solver1 = productOf both . foldl' (flip updatePos) (0,0)

-- TODO Can we just lens into the state and eval after that?
solver2 :: [Instruction] -> Int
solver2 l = productOf both $ evalState (foldM (flip updatePos2) (0, 0) l) 0

main :: IO ()
main = solve day parser solver1 solver2
