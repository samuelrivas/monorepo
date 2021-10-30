{-# LANGUAGE NoImplicitPrelude #-}
{-| Utilities to parse Adventofcode inputs
-}
module Text.Parsec.Advent (
  -- * Common parsers
  listOfNum,
  -- * Parsing functions
  getParsedInput
  ) where

import           Perlude

import           Data.Advent          (Day)
import           System.IO.Advent     (getInput)
import           Text.Parsec          (sepEndBy)
import           Text.Parsec.Char     (char)
import           Text.Parsec.Parselib (num, unsafeParseAll)
import           Text.Parsec.Text     (Parser)

-- | Parse new line separated integers into a list of 'Num'
listOfNum :: Num n => Parser [n]
listOfNum = num `sepEndBy` char '\n'

-- | Given a suitable 'Parser', get the parsed input for a given 'Day'
getParsedInput :: MonadIO m => MonadFail m => Day -> Parser a -> m a
getParsedInput d p = getInput d >>= unsafeParseAll p
