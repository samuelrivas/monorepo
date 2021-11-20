{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day1 where

import           Perlude

import           Data.Advent          (Day (..))
import           System.IO.Advent     (getInput, solve)
import           Text.Parsec.Parselib (Parser)

day :: Day
day = D1

rawInput :: IO Text
rawInput = getInput day

parser :: Parser ()
parser = return ()

solver1 :: () -> Text
solver1 = const "N/A"

solver2 :: () -> Text
solver2 = const "N/A"

main :: IO ()
main = solve day parser solver1 solver2

