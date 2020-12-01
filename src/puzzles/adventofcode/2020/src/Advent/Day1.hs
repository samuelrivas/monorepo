{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day1 where

import           Prelude          hiding (putStrLn, read)
import qualified Prelude

import           Data.List        (find, tails)
import           Data.Maybe       (fromJust)
import           Data.Text        (Text, splitOn, unpack)
import           Data.Text.IO     (putStrLn)

import qualified System.IO.Advent as IOAdvent

targetValue :: Int
targetValue = 2020

example :: [Int]
example = [1721, 979, 366, 299, 675, 1456]

-- TODO: Move read :: Text -> a to our own prelude
read :: Read a => Text -> a
read = Prelude.read . unpack

-- TODO :: Move splitOn read to Advent
getInput :: IO [Int]
getInput =  fmap read . init . splitOn "\n" <$> IOAdvent.getInput "1"

pairs :: [Int] -> [(Int, Int)]
pairs l = zip l l

-- TODO: Generalise to traversable
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = return []
combinations n xs = do
  y:xs' <- tails xs
  ys <- combinations (n-1) xs'
  return (y:ys)

-- TODO: Prettyfy with a lens fold
solve1 :: [Int] -> Maybe Int
solve1 = fmap product <$> find ((== targetValue) . sum) . combinations 2

-- TODO: Prettyfy with a lens fold
solve2 :: [Int] -> Maybe Int
solve2 = fmap product <$> find ((== targetValue) . sum) . combinations 3

main :: IO ()
main =
  do
    input <- getInput
    let
      sol1 = fromJust . solve1 $ input
      sol2 = fromJust . solve2 $ input

    print sol1
    print sol2
