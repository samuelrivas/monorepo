{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Advent.Day22 (main) where

import           Perlude

import           Advent.Templib       (Metrics (..), MonadEmit, emit, linesOf,
                                       solveM)

import           Control.Applicative  ((<|>))
import           Control.Lens         (Each (each), Lens', _1, _2, _3, allOf,
                                       over, set, view)
import           Control.Monad.State  (MonadState (get), evalStateT, execStateT,
                                       gets, modify)
import           Data.Advent          (Day (..))
import           Data.Foldable        (traverse_)
import           Data.Functor         (($>))
import           Data.Generics.Labels ()
import qualified Data.HashMap.Strict  as HashMap
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HashSet
import           Data.Ix              (inRange)
import           Data.Maybe           (fromJust, listToMaybe, mapMaybe)
import           Data.Monoid          (Sum (..))
import           Data.Text            (intercalate)
import           System.IO.Advent     (getInput, getParsedInput)
import           Text.Parsec          (try)
import           Text.Parsec.Parselib (Parser, literal, num, unsafeParseAll)

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
  isOn <- try (literal "on ") $> True <|> literal "off " $> False
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

runInstruction ::
  MonadEmit CubeMetrics m => MonadState (HashSet (Coord, Coord))
  m => Instruction -> m ()
runInstruction (True, fromCoord, toCoord) =
  countInstructionOn >> setOn (fromCoord, toCoord)
runInstruction (False, fromCoord, toCoord) =
  countInstructionOff >> setOff (fromCoord, toCoord)

cubeInRange :: (Coord, Coord) -> Bool
cubeInRange = allOf (each . each) (inRange (-50, 50))

instructionInRange :: Instruction -> Bool
instructionInRange i = cubeInRange (view _2 i, view _3 i)

-- coversSegment a b is true if b overlaps in any way with a
coversSegment :: (Int, Int) -> (Int, Int) -> Bool
coversSegment (x1, x2) (y1, y2) =
  x1 <= y1 && y1 <= x2
  || x1 <= y2 && y2 <= x2
  || y1 <= x1 && x2 <= y2

-- Projects a cube on a single dimension, provided by an accessor
projectCube :: Lens' (Int, Int, Int) Int -> (Coord, Coord) -> (Int, Int)
projectCube accessor cube =
  (view (_1 . accessor) cube, view (_2 . accessor) cube) :: (Int, Int)

-- coversCube a b is true if b overlaps in any way with a
coversCube :: (Coord, Coord) -> (Coord, Coord) -> Bool
coversCube  = allProjections coversSegment

-- allProjections p a b is true if p a b is true for all projections of a and b
allProjections ::
  ((Int, Int) -> (Int, Int) -> Bool) -> (Coord, Coord) -> (Coord, Coord) -> Bool
allProjections f a b =
  f (projectCube _1 a) (projectCube _1 b)
  && f (projectCube _2 a) (projectCube _2 b)
  && f (projectCube _3 a) (projectCube _3 b)

-- TODO This reeks as something that can be done in a simpler way
--
-- splitSegment a b cuts b in chunks so that all of them are either fully inside
-- a or fully outside a
splitSegment :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
splitSegment splitter@(a1, a2) segment@(b1, b2)
  | a2 < b1 || b2 < a1 = [segment] -- disjoint
  | a1 <= b1 && b2 <= a2 = [segment] -- splitter contains segment
  | b1 < a1 && a2 < b2 = [(b1, a1 - 1), splitter, (a2 + 1, b2)] -- segment cotains splitter
  | a2 < b2 = [(b1, a2), (a2 + 1, b2)] -- spiltter contains left end of segment
  | a1 > b1 = [(b1, a1 - 1), (a1, b2)] -- splitter contains right end of segment
  | otherwise = error "unexpected segments"

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
countCells ((x, y, z), (x', y', z')) =
  (x' - x + 1) * (y' - y + 1) * (z' - z + 1)

-- Returns a set of non overlapping cubes covering all volume that is in b but
-- not in a
difference :: (Coord, Coord) -> (Coord, Coord) -> HashSet (Coord, Coord)
difference a b =
  HashSet.filter (not . subcube a) (HashSet.fromList $ breakBy a b)

-- subsegment a b returns whether b is fully included in a
subsegment :: (Int, Int) -> (Int, Int) -> Bool
subsegment (x1, x2) (x1', x2') =
  x1 <= x1' && x1' <= x2
  && x1 <= x2' && x2' <= x2

-- subcube a b returns whether b is fully included in a
subcube :: (Coord, Coord) -> (Coord, Coord) -> Bool
subcube = allProjections subsegment

unions :: (Coord, Coord) -> HashSet (Coord, Coord) -> HashSet (Coord, Coord)
unions cube = HashSet.insert cube . differences cube

-- Returns a set of non overlapping cubes that cover all the volume of the
-- provided set that is not covered by cube
differences ::
  (Coord, Coord) -> HashSet (Coord, Coord) -> HashSet (Coord, Coord)
differences cube =
  HashSet.fromList . concatMap (HashSet.toList . difference cube)

-- Tries to merge two cubes across a dimension. Works if b's merge coord is 1 +
-- a's merge coord, so if you don't know how the segments are aligned you need to
-- run this operation both ways
tryMerge' ::
  Lens' (Int, Int, Int) Int
 -> Lens' (Int, Int, Int) Int
 -> Lens' (Int, Int, Int) Int
 -> (Coord, Coord)
 -> (Coord, Coord)
 -> Maybe (Coord, Coord)
tryMerge' mergeAccessor keepAccessor1 keepAccessor2 a b =
  let
    (_, a2) = projectCube mergeAccessor a
    (b1, _) = projectCube mergeAccessor b
  in
    if projectCube keepAccessor1 a /= projectCube keepAccessor1 b
       || projectCube keepAccessor2 a /= projectCube keepAccessor2 b
       || b1 /= a2 + 1
    then Nothing
    else Just (view _1 a, view _2 b)

-- This tries to merge just in one direction, merge a b succeeds if b's left
-- coord is a's right coord + 1 across the merging edge. If you don't know how
-- the cubes are aligned you need to run this operation both ways
tryMerge :: (Coord, Coord) -> (Coord, Coord) -> Maybe (Coord, Coord)
tryMerge a b =
      tryMerge' _1 _2 _3 a b
  <|> tryMerge' _2 _1 _3 a b
  <|> tryMerge' _3 _1 _2 a b

-- Tries to merge one pair of cubes in the state set
reduceStep :: MonadState (HashSet (Coord, Coord)) m => m Bool
reduceStep = do
  getReducible >>= \case
    Just (a, b, merged) -> do
      modify (HashSet.delete a)
      modify (HashSet.delete b)
      modify (HashSet.insert merged)
      pure True
    Nothing -> pure False

-- TODO Move to library
firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

getReducible ::
  MonadState (HashSet (Coord, Coord)) m
  => m (Maybe ((Coord, Coord), (Coord, Coord), (Coord, Coord)))
getReducible =
  do
    cubes <- gets HashSet.toList
    let pairs = [(x, y) | x <- cubes, y <- cubes, x /= y]
    pure $ firstJust (\(a, b) -> (a, b,) <$> tryMerge a b) pairs

-- Keeps merging pairs of cubes until there is no more cubes that can be merged
mergeCubes ::
  MonadEmit CubeMetrics m => MonadState (HashSet (Coord, Coord)) m => m ()
mergeCubes = do
  reduceStep >>= \case
    True  -> countMerge >> mergeCubes
    False -> pure ()

type CubeMetrics = Metrics (Sum Int)

countInstruction :: MonadEmit CubeMetrics m => m ()
countInstruction = emit . Metrics . HashMap.singleton "instructions" . Sum $ 1

countInstructionOn :: MonadEmit CubeMetrics m => m ()
countInstructionOn = emit . Metrics . HashMap.singleton "ons" . Sum $ 1

countInstructionOff :: MonadEmit CubeMetrics m => m ()
countInstructionOff = emit . Metrics . HashMap.singleton "offs" . Sum $ 1

countAddedCubes :: MonadEmit CubeMetrics m => Int -> m ()
countAddedCubes = emit . Metrics . HashMap.singleton "added cubes" . Sum

countRemovedCubes :: MonadEmit CubeMetrics m => Int -> m ()
countRemovedCubes = emit . Metrics . HashMap.singleton "removed cubes" . Sum

countMerge :: MonadEmit CubeMetrics m => m ()
countMerge = emit . Metrics . HashMap.singleton "merges" . Sum $ 1

runReboot :: MonadEmit CubeMetrics m => [Instruction] -> m (HashSet (Coord, Coord))
runReboot instructions =
  evalStateT
  (traverse_ (\x -> runInstruction x >> countInstruction >> mergeCubes) instructions >> get)
  HashSet.empty

-- TODO Merge these two functions
setOff ::
  MonadEmit CubeMetrics m => MonadState (HashSet (Coord, Coord)) m
  => (Coord, Coord) -> m ()
setOff = setOnOrOff differences

setOn ::
  MonadEmit CubeMetrics m => MonadState (HashSet (Coord, Coord)) m
  => (Coord, Coord) -> m ()
setOn = setOnOrOff unions

setOnOrOff ::
  MonadEmit CubeMetrics m => MonadState (HashSet (Coord, Coord)) m
  => ((Coord, Coord) -> HashSet (Coord, Coord) -> HashSet (Coord, Coord))
  -> (Coord, Coord)
  -> m ()
setOnOrOff f cube = do
  affected <- getAffected cube
  modify $ flip HashSet.difference affected

  countRemovedCubes $ HashSet.size affected

  let newCubes = f cube affected
  toInsert <- execStateT mergeCubes newCubes

  countAddedCubes $ HashSet.size toInsert

  modify $ HashSet.union toInsert

getAffected ::
  MonadEmit CubeMetrics m => MonadState (HashSet (Coord, Coord)) m
  => (Coord, Coord) -> m (HashSet (Coord, Coord))
getAffected cube =
  let
    expanded = over (_1 . each) (subtract 1) . over (_2 . each) (+1) $ cube
  in
    gets $ HashSet.filter (coversCube expanded)

solver1 :: MonadEmit CubeMetrics m => Parsed -> m Int
solver1 =
  fmap (sum . fmap countCells . HashSet.toList)
  . runReboot
  . filter instructionInRange

solver2 :: MonadEmit CubeMetrics m => Parsed -> m Int
solver2 = fmap (sum . fmap countCells . HashSet.toList) . runReboot

main :: IO ()
main = solveM day parser solver1 solver2
