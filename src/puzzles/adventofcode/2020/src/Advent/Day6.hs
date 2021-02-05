{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Advent.Day6 where

import           Advent.Perlude

import           Data.List             (foldl1')
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Text             (unpack)
import           Text.Parsec           (char, noneOf, sepBy, sepEndBy)
import           Text.Parsec.Text      (Parser)

import           Advent.Templib        (Day (..), getInput', getParsedInput)
import           Advent.Templib.Parsec (text1)

day :: Day
day = D6

getInput :: IO Text
getInput = getInput' day

example :: Text
example = "abc\n\
          \a\n\
          \b\n\
          \c\n\
          \\n\
          \ab\n\
          \ac\n\
          \\n\
          \a\n\
          \a\n\
          \a\n\
          \a\n\
          \\n\
          \b\n"

parseAnswers :: Parser [Text]
parseAnswers = text1 (noneOf "\n") `sepEndBy` char '\n'

parser :: Parser [[Text]]
parser = parseAnswers `sepBy` char '\n'

toSets :: [[Text]] -> [[Set Char]]
toSets = fmap (fmap (Set.fromList . unpack))

group :: (Set a -> Set a -> Set a) -> [[Set a]]  -> [Set a]
group f = fmap $ foldl1' f

count :: [Set a] -> Int
count = sum . fmap Set.size

main :: IO ()
main = do
  answers <- toSets <$> getParsedInput day parser

  putStr "Solution 1: "
  print $ count $ group Set.union answers

  putStr "Solution 2: "
  print $ count $ group Set.intersection answers
