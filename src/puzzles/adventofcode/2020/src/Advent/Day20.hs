{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day20 where

import           Advent.Perlude

import           Control.Lens     (at, both, each, foldlOf, over, view, _2)
import           Control.Monad    (guard)
import           Data.Bidim       (Bidim, Coord, fromText)
import           Data.List        (find, foldl', sort, unfoldr)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe       (fromJust, isJust)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import qualified Data.Text        as Text
import qualified System.IO.Advent as IOAdvent

getInput :: IO Text
getInput = IOAdvent.getInput "20"

parse :: Text -> [(Int, Bidim Char)]
parse = fmap parseTile . Text.splitOn "\n\n" . Text.strip

parseTile :: Text -> (Int, Bidim Char)
parseTile text =
  let
    (idLine, tileT) = Text.breakOn "\n" text
    idText = fromJust $ Text.stripSuffix ":" idLine >>= Text.stripPrefix "Tile "
  in (read idText, fromText tileT)

main :: IO ()
main = do
  input <- getInput

  putStr "Solution 1: "
  print $ "NA"

  putStr "Solution 2: "
  print $ "NA"
