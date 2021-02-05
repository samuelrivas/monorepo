{-# LANGUAGE OverloadedStrings #-}

module Advent.Day5 where

import           Prelude          hiding (lines, putStr, putStrLn, read, show)

import           Data.List        (find, foldl')
import           Data.Maybe       (fromJust)
import qualified Data.Set         as Set
import           Data.Text        (Text, lines, unpack)
import           Data.Text.IO     (putStr)
import qualified System.IO.Advent as IOAdvent

getInput :: IO Text
getInput = IOAdvent.getInput "5"

toBin :: Text -> [Bool]
toBin = fmap (\x -> x == 'B' || x == 'R') . unpack

binToDec :: [Bool] -> Int
binToDec = foldl' (\acc bit -> fromEnum bit + 2*acc) 0

sitId :: Text -> Int
sitId = binToDec . toBin

solution1 :: [Text] -> Int
solution1 = maximum . fmap sitId

solution2 :: [Text] -> Maybe Int
solution2 texts =
  let
    sits = Set.fromList $ sitId <$> texts
    isMySit x =
      not (Set.member x sits)
      && Set.member (x - 1) sits
      && Set.member (x + 1) sits
  in
    find isMySit [(Set.findMin sits)..(Set.findMax sits)]

main :: IO ()
main = do
  input <- lines <$> getInput

  putStr "Solution 1: "
  print . solution1 $ input

  putStr "Solution 2: "
  print . fromJust . solution2 $ input
