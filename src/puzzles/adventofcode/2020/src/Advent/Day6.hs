{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Advent.Day6 where

import           Advent.Perlude

import           Data.List        (foldl1')
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Text        (splitOn, strip, unpack)
import qualified System.IO.Advent as IOAdvent

getInput :: IO Text
getInput = IOAdvent.getInput "6"

parse :: Text -> [[Text]]
parse = fmap lines . splitOn "\n\n" . strip

example :: [[Text]]
example = [["abc"], ["a", "b", "c"], ["ab", "ac"], ["a", "a", "a"], ["b"]]

toSets :: [[Text]] -> [[Set Char]]
toSets = fmap (fmap (Set.fromList . unpack))

group :: (Set a -> Set a -> Set a) -> [[Set a]]  -> [Set a]
group f = fmap $ foldl1' f

count :: [Set a] -> Int
count = sum . fmap Set.size

main :: IO ()
main = do
  input <- toSets . parse <$> getInput

  putStr "Solution 1: "
  print $ count $ group Set.union input

  putStr "Solution 2: "
  print $ count $ group Set.intersection input

