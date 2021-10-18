{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day2 where


import           Perlude

import           Control.Lens          (ix, preview)
import           Data.Maybe            (fromJust)
import           Data.Text             (Text, count, singleton)
import qualified Data.Text             as Text
import qualified System.IO.Advent      as IOAdvent
import           Text.Parsec           (anyChar, char, endOfLine, noneOf,
                                        sepEndBy, string)
import           Text.Parsec.Text      (Parser)

import           Advent.Templib.Bool   (xor)
import           Advent.Templib.Parsec (digitsAsNum, text, unsafeParseAll)

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
parseEntry =
      (,)
  <$> (parsePolicy <* string ": ")
  <*> text (noneOf "\n")

parsePolicy :: Parser Policy
parsePolicy =
      (,,)
  <$> (digitsAsNum <* char '-')
  <*> (digitsAsNum <* char ' ')
  <*> (Text.singleton <$> anyChar)

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
  entries <- getInput >>= unsafeParseAll parser
  print . length . filter (uncurry valid) $ entries
  print . length . filter (uncurry valid2) $ entries
