{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day2 where

import           Prelude          hiding (lines, putStrLn, read)
import qualified Prelude

import           Data.Text        (Text, count, lines, splitOn, unpack)
import qualified Data.Text        as Text

import           Data.Text.IO     (putStrLn)

import qualified System.IO.Advent as IOAdvent

-- TODO: Move read :: Text -> a to our own prelude
read :: Read a => Text -> a
read = Prelude.read . unpack

example :: Text
example = "1-3 a: abcde\n\
          \1-3 b: cdefg\n\
          \2-9 c: ccccccccc"

getInput :: IO Text
getInput = IOAdvent.getInput "2"

type Spec = (Int, Int, Text)

-- TODO: Write an actual parser here with parsec
parseLine :: Text -> (Spec, Text)
parseLine line =
  let
    [spec, password] = splitOn ": " line
    [range, letter] = splitOn " " spec
    [lowBound, highBound] = read <$> splitOn "-" range

  in ((lowBound, highBound, letter), password)

valid :: Spec -> Text -> Bool
valid (lo, hi, letter) password =
  let n = count letter password
  in (n >= lo) && (n <= hi)

main :: IO ()
main = do
  input <- getInput
  let
    validPasswords = filter (uncurry valid) $ parseLine <$> lines input

  print $ length validPasswords
