{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day3 where

import           Prelude              hiding (lines, putStrLn, read)
import qualified Prelude

import           Control.Lens         (at, view, _2)
import           Control.Monad.Reader (MonadReader, asks)
import           Data.Bidim           (Bidim, Coord, boundaries, fromText, plus)
import           Data.List            (unfoldr)
import           Data.Maybe           (fromJust)
import           Data.Text            (Text, count, lines, singleton, splitOn,
                                       unpack)
import qualified Data.Text            as Text
import           Data.Text.IO         (putStrLn)
import qualified System.IO.Advent     as IOAdvent

-- TODO: Move read :: Text -> a to our own prelude
read :: Read a => Text -> a
read = Prelude.read . unpack

example :: Text
example = "..##.......\n\
          \#...#...#..\n\
          \.#....#..#.\n\
          \..#.#...#.#\n\
          \.#...##..#.\n\
          \..#.##.....\n\
          \.#.#.#....#\n\
          \.#........#\n\
          \#.##...#...\n\
          \#...##....#\n\
          \.#..#...#.#\n"

getInput :: IO Text
getInput = IOAdvent.getInput "3"

-- TODO: Move to Bidim

width :: Bidim a -> Int
width bidim =
  let ((loX, _), (hiX, _)) = boundaries bidim
  in hiX - loX + 1

height :: Bidim a -> Int
height bidim =
  let ((_, loY), (_, hiY)) = boundaries bidim
  in hiY - loY + 1

wrap :: Bidim a -> Coord -> Coord
wrap bidim (x, y) = (x `mod` width bidim, y `mod` height bidim)

exampleBidim :: Bidim Char
exampleBidim = fromText example

tobogganMove :: Int -> Coord
tobogganMove 1 = (1, 1)
tobogganMove 2 = (3, 1)
tobogganMove 3 = (5, 1)
tobogganMove 4 = (7, 1)
tobogganMove 5 = (1, 2)

step :: Bidim a -> Int -> Coord -> Maybe Coord
step bidim move initialPos =
  let nextPos = wrap bidim $ initialPos `plus` tobogganMove move
  in if view _2 nextPos <= view _2 initialPos
     then Nothing
     else Just nextPos

path :: Bidim a -> Int -> [Coord]
path bidim move =
  let
    f pos = do
      nextPos <- step bidim move pos
      return (nextPos, nextPos)
  in
    unfoldr f (0, 0)

-- TODO: This is unreadable, arrange it better
countTrees :: Bidim Char -> Int -> Int
countTrees bidim move =
  length . filter (== Just '#') $ flip view bidim . at <$> path bidim move

main :: IO ()
main = do
  bidim <- fromText <$> getInput
  print $ countTrees bidim 1
  print . product $ countTrees bidim <$> [1..5]
