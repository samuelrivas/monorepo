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
  linesOf
  ) where
import           Perlude

import           Control.Applicative  ((<|>))
import           Data.Foldable        (foldl')
import           Data.Functor         (($>))
import           Data.List            (tails)
import           Text.Parsec          (char, many1, sepEndBy)
import           Text.Parsec.Parselib (Parser, literal)

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
