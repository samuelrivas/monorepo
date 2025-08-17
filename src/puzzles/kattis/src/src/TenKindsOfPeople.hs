{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module TenKindsOfPeople where

import           Perlude

import           Control.Monad        (foldM_, when)
import           Control.Monad.ST     (ST)
import           Control.Monad.Zip    (mzip)
import           Data.List.NonEmpty   (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty   as NonEmpty
import           Data.Maybe           (fromJust)
import           Data.Set             (Set)
import           Data.Text            (intercalate)
import           Internal.UnionFind   (MutableUnionFind, new, union)
import           Text.Parsec          (count, manyTill, newline, sepBy, space)
import           Text.Parsec.Parselib (Parser, bit, digitsAsNum)

type Coord = (Int, Int)

data Input = Input {
  rows    :: Int,
  columns :: Int,
  bitmap  :: NonEmpty (NonEmpty Bool),
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

exampleFoo :: Text
exampleFoo =
  intercalate "\n"
  [
    "2 4",
    "1100",
    "0110",
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
    (rows, columns) <- parseCoord <* newline
    bitmap <- parseBitmap rows
    _ :: Int <- digitsAsNum <* newline
    queries <- sepBy parseQuery newline
    return $ Input rows columns bitmap queries

parseCoord :: Parser Coord
parseCoord =
  (,)
  <$> digitsAsNum <* space
  <*> digitsAsNum

parseBitmap :: Int -> Parser (NonEmpty (NonEmpty Bool))
parseBitmap rows = forceNonEmpty <$> count rows (forceNonEmpty <$> manyTill bit newline)

forceNonEmpty :: [a] -> NonEmpty a
forceNonEmpty = fromJust . nonEmpty

parseQuery :: Parser (Coord, Coord)
parseQuery =
  (,)
  <$> parseCoord <* space
  <*> parseCoord

toUFIndex :: Int -> Coord -> Int
toUFIndex columns (x, y) =
  (x - 1) + columns * (y - 1)

toCoord :: Int -> Int -> Coord
toCoord columns n = (n `mod` columns + 1, n `div` columns + 1)

makeUf :: Int -> Int -> NonEmpty (NonEmpty Bool) -> ST s (MutableUnionFind s)
makeUf rows columns bitmap =
  let
    (firstRow :| followingRows) = bitmap
  in do
    uf <- new $ rows * columns
    unionRightwards columns uf (1, 1) firstRow
    unionDownwards columns uf (1, 2) firstRow followingRows
    pure uf

-- TODO: This is very messy, probably a fold can make it clearer, or a better UF
-- interface with monadic behaviour
--
-- Coord is the coordinate of the element in focus
-- Unions consecutive, equal elements in a row, left to right
unionRightwards :: Int -> MutableUnionFind s -> Coord -> NonEmpty Bool -> ST s ()
unionRightwards columns uf coord (h :| t) =
  unionRightwards' columns uf coord h t

unionRightwards' :: Int -> MutableUnionFind s -> Coord -> Bool -> [Bool] -> ST s ()
unionRightwards' columns uf coord current [] = pure ()
unionRightwards' columns uf coord current nexts@(h:t) =
  let
    convert = toUFIndex columns
  in do
    when (current == h) $ union uf (convert coord) (convert $ columnRight coord)
    unionRightwards' columns uf (columnRight coord) h t

columnRight :: Coord -> Coord
columnRight (x, y) = (x + 1, y)

rowUp :: Coord -> Coord
rowUp (x, y) = (x, y - 1)

unionDownwards ::
  Int
  -> MutableUnionFind s
  -> Coord
  -> NonEmpty Bool
  -> [NonEmpty Bool]
  -> ST s ()
unionDownwards columns uf coord previousRow [] = pure ()
unionDownwards columns uf coord previousRow (currentRow:t) =
  do
    unionRightwards columns uf coord currentRow
    unionRows columns uf coord previousRow currentRow

unionRows ::
  Int
  -> MutableUnionFind s
  -> Coord
  -> NonEmpty Bool
  -> NonEmpty Bool
  -> ST s ()
unionRows columns uf coord previousRow currentRow =
  let
    convert = toUFIndex columns
    zipped = mzip previousRow currentRow
    f coord' (prev, cur) =
      do
        when (prev == cur) $ union uf (convert  coord') (convert $ rowUp coord')
        return $ columnRight coord'
  in
    foldM_ f coord zipped
