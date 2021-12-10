{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day4 where

import           Perlude

import           Advent.Day4.Internal
import           Advent.Templib       (binToDec, linesOf, matrix)

import           Control.Lens         (_1, _2, _3, _Just, _head, allOf, at,
                                       filtered, folded, non, none, over,
                                       preview, to, toListOf, use, view)
import           Control.Monad        (filterM)
import           Control.Monad.Loops  (iterateUntilM, iterateWhile)
import           Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import           Control.Monad.State  (MonadState, State, StateT, gets, modify,
                                       runState, runStateT)
import           Data.Advent          (Day (..))
import           Data.Bidim           (Bidim)
import           Data.Foldable        (find, foldl', traverse_)
import           Data.Functor         (($>))
import           Data.Generics.Labels ()
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HashMap
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HashSet
import           Data.List            (nub, transpose)
import           Data.Map.Strict      (Map)
import           Data.Maybe           (fromJust, isJust)
import           Data.Text            (intercalate)
import           System.IO.Advent     (getInput, solve)
import           Text.Parsec          (char, many1, sepBy, sepEndBy, (<|>))
import           Text.Parsec.Bidim    (bidim)
import           Text.Parsec.Parselib (Parser, digitsAsNum, literal,
                                       unsafeParseAll)

type Parsed =  ([Int], [[[Int]]])

day :: Day
day = D4

rawInput :: IO Text
rawInput = getInput day

example :: Text
example = intercalate "\n" [
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
  "",
  "22 13 17 11  0",
  " 8  2 23  4 24",
  "21  9 14 16  7",
  " 6 10  3 18  5",
  " 1 12 20 15 19",
  "",
  " 3 15  0  2 22",
  " 9 18 13 17  5",
  "19  8  7 25 23",
  "20 11 10 24  4",
  "14 21 16 12  6",
  "",
  "14 21 17 24  4",
  "10 16 15  9 19",
  "18  8 23 26 20",
  "22 11 13  6  5",
  " 2  0 12  3  7"
  ]

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parser :: Parser Parsed
parser =
  (,)
  <$> digitsAsNum `sepBy` char ',' <* literal "\n\n"
  <*> linesOf (matrix digitsAsNum)

mkBoards :: [[[Int]]] -> Boards
mkBoards matrices =
  let
    dims = dimmensions matrices
    withCoordinates =
      zip (coordinates dims) (toListOf (traverse . traverse . traverse) matrices)
  in
    Boards
    (HashMap.fromList withCoordinates)
    (foldl' (\i (pos, x) -> HashMap.insertWith HashSet.union x pos i)
      HashMap.empty (over _1 HashSet.singleton <$> withCoordinates))


dimmensions :: [[[Int]]] -> Coord
dimmensions l =
    (length . head . head $ l,  length . head $ l, length l)

-- TODO Move to library, possibly generalised
coordinates :: Coord ->  [Coord]
coordinates (x, y, z) =
  let
    count = [0..]
  in
    zip3
    ((`mod` x) <$> count)
    ((`mod` y) . (`div` x) <$> count)
    ((`mod` z) . (`div` (y * x)) <$> count)

-- type BoardIndex = HashMap Int (HashSet Coord)

punch :: MonadState BingoState m => Coord -> m ()
punch coord = modify $ HashSet.insert coord

-- TODO I am hardcoding the dimmensions here, which sucks and is wrong to boot
-- Also the (: []) isn't great either, this may be more readable using some
-- lensy fold/traversal
row :: Coord -> [Coord]
row = (over _1 . const <$> [0..4] <*>) . (: [])

column :: Coord -> [Coord]
column = (over _2 . const <$> [0..4] <*>) . (: [])

-- Returns the punched holes
drawNumber ::
  MonadState BingoState m =>
  MonadReader Boards m =>
  Int -> m (HashSet Coord)
drawNumber n = do
  coords <- view (#inverse . at n . non HashSet.empty)
  traverse_ punch (HashSet.toList coords)
  pure coords

checkWin :: MonadState BingoState m =>
  Coord -> m (Maybe [Coord])
checkWin coord =
  let
    rowCoords = row coord
    columnCoords = column coord
  in do
    hasColumn <- all isJust <$> traverse (use . at) columnCoords
    hasRow <- all isJust <$> traverse (use . at) rowCoords
    if hasColumn then
      pure . Just $ columnCoords
    else if hasRow then
      pure . Just $ rowCoords
    else
      pure Nothing

drawAndCheck ::
  MonadState BingoState m =>
  MonadReader Boards m =>
  Int -> m (Int, [[Coord]])
drawAndCheck n = do
  punchedCoords <- drawNumber n
  (n,) . toListOf (folded . _Just) <$>
    traverse checkWin (HashSet.toList punchedCoords)

findWin ::
  MonadState BingoState m =>
  MonadReader Boards m =>
  [Int] -> m (Int, [[Coord]])
findWin [] = undefined
findWin (h:t) = do
  drawAndCheck h >>= \case
    (_, [])          -> findWin t
    (_, rowOrColumn) -> pure (h, rowOrColumn)

unmarkedNumbers ::
 MonadState BingoState m =>
 MonadReader Boards m =>
 Int -> m [Int]
unmarkedNumbers board = do
  coords <- filterM (fmap not . isPunched) [(x, y, board) | x <- [0..4], y <- [0..4]]
  let
    getNumber c = fromJust <$> view (#direct . at c)
  traverse getNumber coords

isPunched :: MonadState BingoState m => Coord -> m Bool
isPunched coord = gets $ HashSet.member coord

runBingo :: Boards -> ReaderT Boards (State BingoState) a -> (a, BingoState)
runBingo index x = runState (runReaderT x index) HashSet.empty

solver1 :: Parsed -> Int
solver1 (numbers, boards) =
  view _1 . runBingo (mkBoards boards) $ do
  (n, coords) <- findWin numbers
  let board = fromJust $ preview (_head . _head . _3) coords
  (* n) . sum <$> unmarkedNumbers board

solver2 :: Parsed -> [Int]
solver2 (numbers, boards) =
  nub $ toListOf (_1 . traverse . _2 . filtered (not . null) . traverse . traverse . _3 ) $ runBingo (mkBoards boards) (traverse drawAndCheck numbers)


main :: IO ()
main = solve day parser solver1 solver2
