{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module Advent.Day22 where

import           Perlude

import           Advent.Templib                  (linesOf)

import           Control.Applicative             ((<|>))
import           Control.Lens                    (Each (each), Getting, Prism',
                                                  _1, _2, _3, _Just, both,
                                                  preview, prism, sumOf,
                                                  toListOf, view)
import           Control.Monad.State             (MonadState (get), evalState,
                                                  modify)
import           Data.Advent                     (Day (..))
import           Data.Foldable                   (traverse_)
import           Data.Functor                    (($>))
import           Data.Generics.Labels            ()
import           Data.HashSet                    (HashSet)
import qualified Data.HashSet                    as HashSet
import           Data.List                       (foldl1', sort)
import           Data.Maybe                      (fromJust)
import           Data.Text                       (intercalate)
import           Distribution.Types.VersionRange (VersionRangeF (VersionRangeParensF))
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

runInstruction :: MonadState (HashSet Coord) m => Instruction -> m ()
runInstruction (True, fromCoord, toCoord)  = turnCubes HashSet.union fromCoord toCoord
runInstruction (False, fromCoord, toCoord) = turnCubes HashSet.difference fromCoord toCoord

turnCubes ::
  MonadState (HashSet Coord) m
  => (HashSet Coord -> HashSet Coord -> HashSet Coord) -> Coord -> Coord -> m ()
turnCubes f fromCoord toCoord =
  let
    xs = mkRange _1 fromCoord toCoord
    ys = mkRange _2 fromCoord toCoord
    zs = mkRange _3 fromCoord toCoord
    allCoords = [(x,y,z) | x <- xs, y <- ys, z <- zs]
  in
    modify (`f` HashSet.fromList allCoords)

mkRange :: Getting Int Coord Int -> Coord -> Coord -> [Int]
mkRange accessor fromCoord toCoord =
  case (view accessor fromCoord, view accessor toCoord) of
    (x, y) | x <= y -> limitRange x y
           | otherwise -> limitRange y x

limitRange :: Int -> Int -> [Int]
limitRange x y =
  let
    x' = if x < -50 then -50 else x
    y' = if y > 50 then 50 else y
  in
    [x'..y']

runReboot :: [Instruction] -> HashSet (Coord)
runReboot instructions =
  evalState (traverse_ runInstruction instructions >> get) HashSet.empty

solver1 :: Parsed -> Int
solver1 = HashSet.size . runReboot

solver2 :: Parsed -> Int
solver2 = undefined

main :: IO ()
main = solve day parser solver1 solver2
