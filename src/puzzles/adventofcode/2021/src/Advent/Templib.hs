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
  conv,
  solveM,
  linesOf,
  bitString
  ) where
import           Perlude


import           Control.Monad.MonadEmit (EmitTVarT',
                                          runEmitTVarTWithPrinterThread')
import           Data.Advent             (Day)
import           Data.Generics.Labels    ()
import           Data.List               (tails)
import           System.IO.Advent        (getParsedInput)
import           Text.Parsec.Parselib    (Parser, bitString, linesOf)
import           UnliftIO                (MonadUnliftIO)

-- TODO use this in day 2 or delete
conv :: ([a] -> b) -> [a] -> [b]
conv f l = f <$> tails l

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
