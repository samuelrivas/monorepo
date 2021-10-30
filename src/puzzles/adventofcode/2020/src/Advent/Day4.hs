{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day4 where

import           Perlude

import           Control.Monad        (guard)
import           Data.Map             (Map, assocs, keysSet)
import qualified Data.Map             as Map
import           Data.Maybe           (isJust)
import           Data.Set             (Set, difference, member)
import qualified Data.Set             as Set
import           Data.Text            (Text, dropEnd, takeEnd, unpack)
import qualified Data.Text            as Text
import qualified System.IO.Advent     as IOAdvent
import           Text.Parsec          (char, letter, noneOf, oneOf, sepBy,
                                       sepEndBy)
import           Text.Parsec.Parselib (text, unsafeParseAll)
import           Text.Parsec.Text     (Parser)
import qualified Text.Read            as Read


exampleInvalid :: Text
exampleInvalid = "eyr:1972 cid:100\n\
                 \hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\
                 \\n\
                 \iyr:2019\n\
                 \hcl:#602927 eyr:1967 hgt:170cm\n\
                 \ecl:grn pid:012533040 byr:1946\n\
                 \\n\
                 \hcl:dab227 iyr:2012\n\
                 \ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\
                 \\n\
                 \hgt:59cm ecl:zzz\n\
                 \eyr:2038 hcl:74454a iyr:2023\n\
                 \pid:3556412378 byr:2007\n"

exampleValid :: Text
exampleValid = "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\n\
               \hcl:#623a2f\n\
               \\n\
               \eyr:2029 ecl:blu cid:129 byr:1989\n\
               \iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\
               \\n\
               \hcl:#888785\n\
               \hgt:164cm byr:2001 iyr:2015 cid:88\n\
               \pid:545766238 ecl:hzl\n\
               \eyr:2022\n\
               \\n\
               \iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719\n"

getInput :: IO Text
getInput = IOAdvent.getInput "4"

type Passport = Map Text Text

formatPassport :: Passport -> Text
formatPassport pass =
  let formatEntry (k,v) = show k <> ": " <> show v
  in
    Text.unlines $ formatEntry <$> Map.assocs pass

parser :: Parser [Passport]
parser = parsePassport `sepBy` char '\n'

parsePassport :: Parser Passport
parsePassport = Map.fromList <$> parseEntry `sepEndBy` oneOf " \n"

parseEntry :: Parser (Text, Text)
parseEntry = (,)
  <$> (text letter <* char ':')
  <*> text (noneOf " \n")

-- Ignoring cid here
mandatoryKeys :: Set Text
mandatoryKeys = Set.fromList
  [ "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"
  ]

-- TODO: This is part of the most recent base (for String), make it for Text in
-- our prelude
readMaybe :: Read a => Text -> Maybe a
readMaybe = Read.readMaybe . unpack

hasMandatoryKeys :: Passport -> Bool
hasMandatoryKeys passport = null $ difference mandatoryKeys (keysSet passport)

validateField :: Text -> Text -> Bool
validateField "byr" t = isJust $ do
  n :: Int <- readMaybe t
  guard $ (n >= 1920) && (n <= 2002)
validateField "iyr" t = isJust $ do
  n :: Int <- readMaybe t
  guard $ (n >= 2010) && (n <= 2020)
validateField "eyr" t = isJust $ do
  n :: Int <- readMaybe t
  guard $ (n >= 2020) && (n <= 2030)
validateField "hgt" t =
  let units = takeEnd 2 t
  in isJust $ do
    n :: Int <- readMaybe $ dropEnd 2 t
    case units of
      "cm" -> guard $ (n >= 150) && (n <= 193)
      "in" -> guard $ (n >= 59) && (n <= 76)
      _    -> Nothing
validateField "hcl" t =
  let validChars = Set.fromList $ ['0'..'9'] ++ ['a'..'f']
  in isJust $ do
    guard $ Text.length t == 7
    guard $ Text.take 1 t == "#"
    guard . Text.all (`member` validChars) . Text.drop 1 $ t
validateField "ecl" t =
  let validCls = Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  in member t validCls
validateField "pid" t =
  let validChars = Set.fromList ['0'..'9']
  in Text.length t == 9 && Text.all (`member` validChars) t
validateField "cid" _ = True
validateField _ _ = False

hasValidFields :: Passport -> Bool
hasValidFields passport = all (uncurry validateField) $ assocs passport

main :: IO ()
main = do
  passports <- getInput >>= unsafeParseAll parser

  putStr "Solution 1: "
  print . length . filter hasMandatoryKeys $ passports

  putStr "Solution 2: "
  print . length . filter hasValidFields . filter hasMandatoryKeys $ passports
