{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Templib (
  binToDec,
  conv,
  linesOf
  ) where
import           Perlude

import           Data.Foldable        (foldl')
import           Data.List            (tails)
import           Text.Parsec          (char, sepEndBy)
import           Text.Parsec.Parselib (Parser)

-- TODO use this in day 2 or delete
conv :: ([a] -> b) -> [a] -> [b]
conv f l = f <$> tails l

binToDec :: [Bool] -> Int
binToDec = foldl' (\acc b -> fromEnum b + 2*acc) 0

-- Parsers

linesOf :: Parser a -> Parser [a]
linesOf p = p `sepEndBy` char '\n'
