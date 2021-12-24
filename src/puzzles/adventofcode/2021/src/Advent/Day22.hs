{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module Advent.Day22 where

import           Perlude

import           Advent.Templib                  (linesOf)

import           Control.Applicative             ((<|>))
import           Control.Lens                    (Each (each), Getting, Lens',
                                                  Prism', _1, _2, _3, _Just,
                                                  allOf, both, preview, prism,
                                                  set, sumOf, toListOf, view)
import           Control.Monad.State             (MonadState (get), evalState,
                                                  modify)
import           Data.Advent                     (Day (..))
import           Data.Foldable                   (traverse_)
import           Data.Functor                    (($>))
import           Data.Generics.Labels            ()
import           Data.HashSet                    (HashSet)
import qualified Data.HashSet                    as HashSet
import           Data.Ix                         (inRange)
import           Data.List                       (foldl1', nub, sort)
import           Data.Maybe                      (fromJust)
import           Data.Text                       (intercalate)
import           Distribution.Types.VersionRange (VersionRangeF (VersionRangeParensF))
import           Numeric.Lens                    (subtracting)
import qualified Prelude
import           System.IO.Advent                (getInput, getParsedInput,
                                                  solve)
import           Text.Parsec                     (char, try)
import           Text.Parsec.Parselib            (Parser, digitAsNum,
                                                  digitsAsNum, literal, num,
                                                  unsafeParseAll)

type Coord = (Int, Int, Int)
type Instruction = (Bool, Coord, Coord)
type Parsed = [Instruction]

day :: Day
day = D22

rawInput :: IO Text
rawInput = getInput day

example :: Text
example =
  intercalate
  "\n" [
  "on x=10..12,y=10..12,z=10..12",
  "on x=11..13,y=11..13,z=11..13",
  "off x=9..11,y=9..11,z=9..11",
  "on x=10..10,y=10..10,z=10..10"
  ]

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parsedInput :: IO Parsed
parsedInput = getParsedInput day parser

parser :: Parser Parsed
parser = linesOf instructionP

instructionP :: Parser Instruction
instructionP = do
  isOn <- (try (literal "on ") $> True) <|> (literal "off " $> False)
  (start, end) <- rangesP
  pure (isOn, start, end)

rangesP :: Parser (Coord, Coord)
rangesP = do
  (minX, maxX) <- rangeP "x" <* literal ","
  (minY, maxY) <- rangeP "y" <* literal ","
  (minZ, maxZ) <- rangeP "z"
  pure ((minX, minY, minZ), (maxX, maxY, maxZ))

rangeP :: Text -> Parser (Int, Int)
rangeP t =
  (,)
  <$> (literal (t <> "=") *> num)
  <*> (literal ".." *> num)

runInstruction :: MonadState [(Coord, Coord)] m => Instruction -> m ()
runInstruction (True, fromCoord, toCoord) = modify (unions (fromCoord, toCoord))
runInstruction (False, fromCoord, toCoord) = modify (differences (fromCoord, toCoord))

cubeInRange :: (Coord, Coord) -> Bool
cubeInRange = allOf (each . each) (inRange (-50, 50))

instructionInRange :: Instruction -> Bool
instructionInRange i = cubeInRange (view _2 i, view _3 i)

-- coversSegment a b is true if b overlaps in any way with a
coversSegment :: (Int, Int) -> (Int, Int) -> Bool
coversSegment (x1, x2) (y1, y2) =
  (x1 <= y1 && y1 <= x2)
  || (x1 <= y2 && y2 <= x2)
  || (y1 <= x1 && x2 <= y2)

-- Projects a cube on a single dimension, provided by an accessor
projectCube :: Lens' (Int, Int, Int) Int -> (Coord, Coord) -> (Int, Int)
projectCube accessor cube =
  (view (_1 . accessor) cube, view (_2 . accessor) cube) :: (Int, Int)

-- coversCube a b is true if b overlaps in any way with a
coversCube :: (Coord, Coord) -> (Coord, Coord) -> Bool
coversCube  = allProjections coversSegment

allProjections ::
  ((Int, Int) -> (Int, Int) -> Bool) -> (Coord, Coord) -> (Coord, Coord) -> Bool
allProjections f a b =
  f (projectCube _1 a) (projectCube _1 b)
  && f (projectCube _2 a) (projectCube _2 b)
  && f (projectCube _3 a) (projectCube _3 b)

-- TODO This reeks as something that can be done in a simpler way
--
-- splitSegment a b cuts b in chunks so that all of them are either fully inside a or fully outside a
splitSegment :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
splitSegment splitter@(a1, a2) segment@(b1, b2)
  | a2 < b1 || b2 < a1 = [segment] -- disjoint
  | a1 <= b1 && b2 <= a2 = [segment] -- splitter contains segment
  | b1 < a1 && a2 < b2 = [(b1, a1 - 1), splitter, (a2 + 1, b2)] -- segment cotains splitter
  | a2 < b2 = [(b1, a2), (a2 + 1, b2)] -- spiltter contains left end of segment
  | a1 > b1 = [(b1, a1 - 1), (a1, b2)] -- splitter contains right end of segment
  | otherwise = error "unexpected segments"

-- Same as breakBy, but across a dimension set by the lens
breakByAcross ::
  Lens' (Int, Int, Int) Int -> (Int, Int) -> (Coord, Coord)
  -> [(Coord, Coord)]
breakByAcross accessor splitter cube =
  let
    segments = splitSegment splitter (projectCube accessor cube)
    cubify (a, b) = set (_1 . accessor) a . set (_2 . accessor) b $  cube
  in
    cubify <$> segments

countCells :: (Coord, Coord) -> Int
countCells ((x, y, z), (x', y', z')) = (x' - x + 1) * (y' - y + 1) * (z' - z + 1)

-- breakBy a b returns b exploded so that is composed of boxes that don't cross
-- any edge of a
breakBy :: (Coord, Coord) -> (Coord, Coord) -> [(Coord, Coord)]
breakBy a b =
  let
    breakByDimmension (accessor  :: Lens' (Int, Int, Int) Int)
      = breakByAcross accessor (projectCube accessor a)
  in
    breakByDimmension _1 b
  >>= breakByDimmension _2
  >>= breakByDimmension _3

-- Joins two cubes into a list of non overlapping cubes
union :: (Coord, Coord) -> (Coord, Coord) -> [(Coord, Coord)]
union a b = a : difference a b

-- Returns non overlapping cubes covering all volume that is in b but not in a
difference :: (Coord, Coord) -> (Coord, Coord) -> [(Coord, Coord)]
difference a b = filter (not . subcube a) (breakBy a b)

-- subcube a b returns whether b is fully included in a
subcube :: (Coord, Coord) -> (Coord, Coord) -> Bool
subcube = allProjections subsegment

-- subsegment a b returns whether b is fully included in a
subsegment :: (Int, Int) -> (Int, Int) -> Bool
subsegment (x1, x2) (x1', x2') =
  x1 <= x1' && x1' <= x2
  && x1 <= x2' && x2' <= x2

unions :: (Coord, Coord) -> [(Coord, Coord)] -> [(Coord, Coord)]
unions cube cubes = cube : differences cube cubes

differences :: (Coord, Coord) -> [(Coord, Coord)] -> [(Coord, Coord)]
differences cube = concatMap (difference cube)

runReboot :: [Instruction] -> [(Coord, Coord)]
runReboot instructions =
  evalState (traverse_ runInstruction instructions >> get) []

solver1 :: Parsed -> Int
solver1 = sum . fmap countCells . runReboot . filter instructionInRange
-- solver1 = HashSet.size . runReboot . fmap limitRange

solver2 :: Parsed -> Int
solver2 = undefined

main :: IO ()
main = solve day parser solver1 solver2
