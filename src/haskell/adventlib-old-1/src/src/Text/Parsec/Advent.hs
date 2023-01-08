{-# LANGUAGE NoImplicitPrelude #-}
{-| Utilities to parse Adventofcode inputs
-}
module Text.Parsec.Advent (
  listOfNum,
  ) where

import           Perlude

import           Text.Parsec          (sepEndBy)
import           Text.Parsec.Char     (char)
import           Text.Parsec.Parselib (num)
import           Text.Parsec.Text     (Parser)

-- | Parse new line separated integers into a list of 'Num'
listOfNum :: Num n => Parser [n]
listOfNum = num `sepEndBy` char '\n'

