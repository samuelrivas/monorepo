{-| Common parsers for adventofcode problems
-}
module Text.Parsec.Advent (
  listOfNum
  ) where

import           Text.Parsec          (sepEndBy)
import           Text.Parsec.Char     (char)
import           Text.Parsec.Parselib (num)
import           Text.Parsec.Text     (Parser)

listOfNum :: Num n => Parser [n]
listOfNum = num `sepEndBy` char '\n'
