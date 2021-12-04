{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Templib (
  binToDec,
  bitString,
  bit,
  conv,
  linesOf,
  matrix
  ) where
import           Perlude

import           Control.Applicative   ((<|>))
import           Control.Monad.RWS.CPS (RWST, evalRWST)
import           Control.Monad.Reader  (MonadReader)
import           Control.Monad.State   (MonadState)
import           Data.Advent           (Day)
import           Data.Bidim            (Coord)
import           Data.Foldable         (foldl')
import           Data.Functor          (($>))
import           Data.List             (tails)
import           Data.Map.Strict       (Map)
import           System.IO.Advent      (getParsedInput)
import           Text.Parsec           (char, many, many1, sepBy, sepEndBy)
import           Text.Parsec.Parselib  (Parser, literal, num)

-- TODO use this in day 2 or delete
conv :: ([a] -> b) -> [a] -> [b]
conv f l = f <$> tails l

binToDec :: [Bool] -> Int
binToDec = foldl' (\acc b -> fromEnum b + 2*acc) 0

-- Parsers

linesOf :: Parser a -> Parser [a]
linesOf p = p `sepEndBy` char '\n'

bit :: Parser Bool
bit = (literal "1" $> True) <|> (literal "0" $> False)

bitString :: Parser [Bool]
bitString = many1 bit

matrix :: Parser a -> Parser [[a]]
matrix p =
  let cell = many (char ' ') *> p
  in many1 cell `sepEndBy` char '\n'

-- Advent templates

solveM ::
  MonadFail m =>
  MonadIO m =>
  Show a =>
  Show b =>
  Day -> Parser input ->
  RWST input () Text m a ->
  RWST input () Text m b ->
  m ()
solveM day parser solver1 solver2 = do
  input <- getParsedInput day parser

  let solutions = (,) <$> solver1 <*> solver2
  ((solution1, solution2), ()) <- evalRWST solutions input ""

  putStr "Solution 1: "
  print solution1

  putStr "Solution 2: "
  print solution2
