{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module TenKindsOfPeople where

import           Perlude

import           Data.Text            (intercalate)
import           Text.Parsec          (count, manyTill, newline, sepBy, space)
import           Text.Parsec.Parselib (Parser, bit, digitsAsNum)

type Coord = (Int, Int)

data Input = Input {
  bitmap  :: [[Bool]],
  queries :: [(Coord, Coord)]
  } deriving stock Show

example1 :: Text
example1 =
  intercalate "\n"
  [
    "1 4",
    "1100",
    "2",
    "1 1 1 4",
    "1 1 1 1"
  ]

example2 :: Text
example2 =
  intercalate "\n"
  [
    "10 20",
    "11111111111111111111",
    "11000000000000000101",
    "11111111111111110000",
    "11111111111111110000",
    "11000000000000000111",
    "00011111111111111111",
    "00111111111111111111",
    "10000000000000001111",
    "11111111111111111111",
    "11111111111111111111",
    "3",
    "2 3 8 16",
    "8 1 7 3",
    "1 1 10 20"
  ]

parser :: Parser Input
parser =
  do
    (rows, _columns) <- parseCoord <* newline
    bitmap <- parseBitmap rows
    _ :: Int <- digitsAsNum <* newline
    queries <- sepBy parseQuery newline
    return $ Input bitmap queries

parseCoord :: Parser Coord
parseCoord =
  (,)
  <$> digitsAsNum <* space
  <*> digitsAsNum

parseBitmap :: Int -> Parser [[Bool]]
parseBitmap rows = count rows $ manyTill bit newline

parseQuery :: Parser (Coord, Coord)
parseQuery =
  (,)
  <$> parseCoord <* space
  <*> parseCoord
