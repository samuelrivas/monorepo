{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day17 where

import           Advent.Perlude

import           Control.Lens     (at, both, each, foldlOf, over, view, _2)
import           Control.Monad    (guard)
import           Data.List        (find, foldl', sort, unfoldr)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe       (fromJust, isJust)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import qualified Data.Text        as Text
import qualified System.IO.Advent as IOAdvent

example :: Text
example = "foo"

getInput :: IO Text
getInput = IOAdvent.getInput "17"

main :: IO ()
main = do
  input <- getInput

  putStr "Solution 1: "
  print $ "NA"

  putStr "Solution 2: "
  print $ "NA"
