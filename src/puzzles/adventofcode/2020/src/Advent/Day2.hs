{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day2 where

import           Prelude          hiding (lines, putStrLn, read)
import qualified Prelude

import           Control.Lens     (ix, preview)
import           Data.Maybe       (fromJust)
import           Data.Text        (Text, count, lines, singleton, splitOn,
                                   unpack)
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

xor :: Bool -> Bool -> Bool
xor True True   = False
xor False False = False
xor _ _         = True

valid2 :: Spec -> Text -> Bool
valid2 (lo, hi, letter) password =
  fromJust $ do
    first  <- singleton <$> preview (ix (lo - 1)) password
    second <- singleton <$> preview (ix (hi - 1)) password
    return $ (first == letter) `xor` (second == letter)

main :: IO ()
main = do
  input <- getInput
  let
    validPasswords = filter (uncurry valid) $ parseLine <$> lines input
    validPasswords2 = filter (uncurry valid2) $ parseLine <$> lines input

  print $ length validPasswords
  print $ length validPasswords2
