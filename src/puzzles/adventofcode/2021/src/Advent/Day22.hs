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

module Advent.Day22 where

import           Perlude

import           Advent.Templib                  (Metrics (..), MonadEmit, emit,
                                                  linesOf, solveM)

import           Control.Applicative             ((<|>))
import           Control.Concurrent              (forkIO)
import           Control.Lens                    (Each (each), Getting, Lens',
                                                  Prism', _1, _2, _3, _Just,
                                                  allOf, both, preview, prism,
                                                  set, sumOf, toListOf, view)
import           Control.Monad.Loops             (whileM_)
import           Control.Monad.State.Strict      (MonadState (get), evalState,
                                                  evalStateT, gets, modify)
import           Control.Monad.Writer            (MonadWriter)
import           Data.Advent                     (Day (..))
import           Data.Foldable                   (traverse_)
import           Data.Functor                    (($>))
import           Data.Generics.Labels            ()
import qualified Data.HashMap.Strict             as HashMap
import           Data.HashSet                    (HashSet)
import qualified Data.HashSet                    as HashSet
import           Data.Ix                         (inRange)
import           Data.List                       (foldl1', nub, sort)
import           Data.Maybe                      (fromJust, listToMaybe,
                                                  mapMaybe)
import           Data.Monoid                     (Sum (..))
import           Data.Primitive                  (copyMutableByteArrayToAddr)
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

runInstruction :: MonadState (HashSet (Coord, Coord)) m => Instruction -> m ()
runInstruction (True, fromCoord, toCoord) = modify (unions (fromCoord, toCoord))
runInstruction (False, fromCoord, toCoord) = modify (differences (fromCoord, toCoord))

runInstructionM ::
  MonadEmit CubeMetrics m => MonadState (HashSet (Coord, Coord)) m =>
  Instruction -> m ()
runInstructionM (True, fromCoord, toCoord) = modify (unions (fromCoord, toCoord))
runInstructionM (False, fromCoord, toCoord) = modify (differences (fromCoord, toCoord))

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
union :: (Coord, Coord) -> (Coord, Coord) -> HashSet (Coord, Coord)
union a b = HashSet.insert a $ difference a b

-- Returns non overlapping cubes covering all volume that is in b but not in a
difference :: (Coord, Coord) -> (Coord, Coord) -> HashSet (Coord, Coord)
difference a b = HashSet.filter (not . subcube a) (HashSet.fromList $ breakBy a b)

-- subcube a b returns whether b is fully included in a
subcube :: (Coord, Coord) -> (Coord, Coord) -> Bool
subcube = allProjections subsegment

-- subsegment a b returns whether b is fully included in a
subsegment :: (Int, Int) -> (Int, Int) -> Bool
subsegment (x1, x2) (x1', x2') =
  x1 <= x1' && x1' <= x2
  && x1 <= x2' && x2' <= x2

unions :: (Coord, Coord) -> HashSet (Coord, Coord) -> HashSet (Coord, Coord)
unions cube = HashSet.insert cube . differences cube

differences :: (Coord, Coord) -> HashSet (Coord, Coord) -> HashSet (Coord, Coord)
differences cube = HashSet.fromList . concatMap (HashSet.toList . difference cube)

-- Works if b's merge coord is 1 + a's merge coord
tryMerge' :: Lens' (Int, Int, Int) Int -> Lens' (Int, Int, Int) Int -> Lens' (Int, Int, Int) Int -> (Coord, Coord) -> (Coord, Coord) -> Maybe (Coord, Coord)
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
-- coord is a's right coord + 1 across the merging edge
tryMerge :: (Coord, Coord) -> (Coord, Coord) -> Maybe (Coord, Coord)
tryMerge a b =
      tryMerge' _1 _2 _3 a b
  <|> tryMerge' _2 _1 _3 a b
  <|> tryMerge' _3 _1 _2 a b

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

mergeCubes :: MonadState (HashSet (Coord, Coord)) m => m ()
mergeCubes = do
  reduceStep >>= \case
    True  -> mergeCubes
    False -> pure ()

mergeCubesM :: MonadEmit CubeMetrics m => MonadState (HashSet (Coord, Coord)) m => m ()
mergeCubesM = do
  reduceStep >>= \case
    True  -> mergeCubes
    False -> pure ()

runReboot :: [Instruction] -> HashSet (Coord, Coord)
runReboot instructions =
  evalState
  (traverse_ (\x -> runInstruction x >> mergeCubes) instructions >> get)
  HashSet.empty

type CubeMetrics = Metrics (Sum Int)

countInstruction :: MonadEmit CubeMetrics m => m ()
countInstruction = emit . Metrics . HashMap.singleton "instructions" . Sum $ 1

runRebootM :: MonadEmit CubeMetrics m => [Instruction] -> m (HashSet (Coord, Coord))
runRebootM instructions =
  evalStateT
  (traverse_ (\x -> runInstructionM x >> countInstruction >> mergeCubesM) instructions >> get)
  HashSet.empty

solver1 :: Parsed -> Int
solver1 = sum . fmap countCells . HashSet.toList . runReboot . filter instructionInRange

-- TODO: This won't finish (Possibly because we are using State.Lazy!!
solver2 :: Parsed -> Int
solver2 = sum . fmap countCells . HashSet.toList . runReboot

solver1M :: (MonadEmit (Metrics (Sum Int)) m) => Parsed -> m Int
solver1M = fmap (sum . fmap countCells . HashSet.toList) . runRebootM . filter instructionInRange

solver2M :: (MonadEmit (Metrics (Sum Int)) m) => Parsed -> m Int
solver2M = fmap (sum . fmap countCells . HashSet.toList) . runRebootM

main :: IO ()
main = do
  solveM day parser solver1M solver2M
