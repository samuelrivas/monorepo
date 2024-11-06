{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Advent.Templib (
  binToDec,
  bitString,
  bit,
  conv,
  linesOf,
  matrix,
  numListToNum,
  solveM
  ) where
import           Perlude


import           Control.Applicative     ((<|>))
import           Control.Monad.MonadEmit (EmitTVarT',
                                          runEmitTVarTWithPrinterThread')
import           Data.Advent             (Day)
import           Data.Foldable           (foldl')
import           Data.Functor            (($>))
import           Data.Generics.Labels    ()
import           Data.List               (tails)
import           System.IO.Advent        (getParsedInput)
import           Text.Parsec             (char, many, many1, sepEndBy)
import           Text.Parsec.Parselib    (Parser, literal)
import           UnliftIO                (MonadUnliftIO)

-- TODO use this in day 2 or delete
conv :: ([a] -> b) -> [a] -> [b]
conv f l = f <$> tails l

binToDec :: [Bool] -> Int
binToDec = foldl' (\acc b -> fromEnum b + 2*acc) 0

numListToNum :: Num x => [x] -> x
numListToNum = foldl (\acc x -> acc * 10 + x) 0

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
  MonadUnliftIO m =>
  Monoid metrics1 =>
  Monoid metrics2 =>
  Show metrics1 =>
  Show metrics2 =>
  Show a =>
  Show b =>
  Day ->
  Parser input ->
  (input -> EmitTVarT' metrics1 m a) ->
  (input -> EmitTVarT' metrics2 m b) ->
  m ()
solveM day parser solver1 solver2 = do
  input <- getParsedInput day parser

  solution1 <- runEmitTVarTWithPrinterThread' (solver1 input) 1000000
  putStr "Solution 1: "
  print solution1

  solution2 <- runEmitTVarTWithPrinterThread' (solver2 input) 1000000
  putStr "Solution 2: "
  print solution2
