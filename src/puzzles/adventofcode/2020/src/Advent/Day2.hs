{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day2 where

import           Prelude               hiding (lines, putStrLn, read)
import qualified Prelude

import           Control.Lens          (ix, preview)
import           Data.Maybe            (fromJust)
import           Data.Text             (Text, count, lines, singleton, splitOn,
                                        unpack)
import qualified Data.Text             as Text
import           Data.Text.IO          (putStrLn)
import qualified System.IO.Advent      as IOAdvent
import           Text.Parsec           (Parsec, anyChar, char, digit, endOfLine,
                                        eof, many, noneOf, sepBy, sepEndBy,
                                        string, (<|>))
import           Text.Parsec.Text      (Parser)

import           Advent.Templib.Bool   (xor)
import           Advent.Templib.Parsec (digitsAsNum, parse, unsafeParse)

-- TODO: Move read :: Text -> a to our own prelude
read :: Read a => Text -> a
read = Prelude.read . unpack

example :: Text
example = "1-3 a: abcde\n\
          \1-3 b: cdefg\n\
          \2-9 c: ccccccccc"

getInput :: IO Text
getInput = IOAdvent.getInput "2"

-- (Min, Max, Character)
type Policy = (Int, Int, Text)

-- (Policy, Password)
type Entry = (Policy, Text)

parser :: Parser [Entry]
parser = parseEntry `sepEndBy` endOfLine

parseEntry :: Parser Entry
parseEntry = do
  policy <- parsePolicy
  _ <- string ": "
  password <- Text.pack <$> many (noneOf "\n")
  pure (policy, password)

parsePolicy :: Parser Policy
parsePolicy = do
  low <- digitsAsNum
  _ <- char '-'
  high <- digitsAsNum
  _ <- char ' '
  character <- Text.singleton <$> anyChar
  pure (low, high, character)

valid :: Policy -> Text -> Bool
valid (lo, hi, letter) password =
  let n = count letter password
  in (n >= lo) && (n <= hi)

valid2 :: Policy -> Text -> Bool
valid2 (lo, hi, letter) password =
  fromJust $ do
    first  <- singleton <$> preview (ix (lo - 1)) password
    second <- singleton <$> preview (ix (hi - 1)) password
    return $ (first == letter) `xor` (second == letter)

main :: IO ()
main = do
  entries <- getInput >>= unsafeParse parser
  print . length . filter (uncurry valid) $ entries
  print . length . filter (uncurry valid2) $ entries
