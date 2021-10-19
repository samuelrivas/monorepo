{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day16 where

import           Perlude

import           Control.Lens          (view, _1, _2)
import           Control.Monad.Fail    (MonadFail)
import           Data.Generics.Labels  ()
import           Data.List             (foldl', intersect, sort, transpose)
import           Data.Text             (isPrefixOf)
import           Text.Parsec           (between, char, endBy, noneOf, sepBy,
                                        sepBy1, sepEndBy)

import           Advent.Day16.Internal (Rule, mkRule)
import           Advent.Templib        (Day (..), getInput', getParsedInput)
import           Advent.Templib.Parsec (Parser, digitsAsNum, literal, text1)


day :: Day
day = D16

-- TODO: this may be easier by using parsec and parsing into what the solution
-- needs, rather than parsing into a more generic representation of the
-- input. For example, we don't care about our ticket or the field names for
-- part 1, but we spent time parsing them

-- TODO: Move to lib
liftMaybe :: MonadFail m => Maybe a -> m a
liftMaybe = \case
  Just a -> pure a
  Nothing -> fail "maybeToFail did, indeed, fail"

example :: Text
example = "class: 1-3 or 5-7\n\
          \row: 6-11 or 33-44\n\
          \seat: 13-40 or 45-50\n\
          \\n\
          \your ticket:\n\
          \7,1,14\n\
          \\n\
          \nearby tickets:\n\
          \7,3,47\n\
          \40,4,50\n\
          \55,2,20\n\
          \38,6,12\n"

example2 :: Text
example2 = "class: 0-1 or 4-19\n\
           \row: 0-5 or 8-19\n\
           \seat: 0-13 or 16-19\n\
           \\n\
           \your ticket:\n\
           \11,12,13\n\
           \\n\
           \nearby tickets:\n\
           \3,9,18\n\
           \15,1,5\n\
           \5,14,9\n"

getInput :: IO Text
getInput = getInput' D16

parser :: Parser ([Rule], [Int], [[Int]])
parser =
  (,,)
  <$> (parseRule `endBy` char '\n' <* char '\n')
  <*> between (literal "your ticket:\n") (literal "\n\n") parseTicket
  <*> (literal "nearby tickets:\n" *> parseNearby)

parseRule :: Parser Rule
parseRule =
  mkRule
  <$> text1 (noneOf ":\n") <* literal ": "
  <*> (parseRange `sepBy` literal " or ")

parseRange :: Parser (Int, Int)
parseRange = (,) <$> (digitsAsNum <* char '-') <*> digitsAsNum

parseTicket :: Parser [Int]
parseTicket = digitsAsNum `sepBy1` char ','

parseNearby :: Parser [[Int]]
parseNearby = parseTicket `sepEndBy` char '\n'

validField :: [(Int, Int)] -> Int -> Bool
validField rules value = any (\(lo, hi) -> (lo <= value) && (value <= hi)) rules

inInterval :: Int -> (Int, Int) -> Bool
inInterval value (lo, hi) = (lo <= value) && (value <= hi)

matchRule :: Int -> Rule -> Bool
matchRule value = any (inInterval value) . view #ranges

allRanges :: [Rule] -> [(Int, Int)]
allRanges = concatMap (view #ranges)

filterInvalid :: [Rule] -> [[Int]] -> [[Int]]
filterInvalid rules =
  let
    isValid = validField . concatMap (view #ranges) $ rules
  in
    filter $ and . fmap isValid

solve1 :: ([Rule], a, [[Int]]) -> Int
solve1 (rules, _, others) =
  let
    ranges = allRanges rules
  in
    sum . concat $ filter (not . validField ranges) <$> others

-- For a given value, find all valid field names
validNames :: Int -> [Rule] -> [Text]
validNames value = fmap (view #name) <$> filter (matchRule value)

-- For a list of values, find all possible names that apply to all of them
findNames :: [Int] -> [Rule] -> [Text]
findNames values rules =
  let
    (h:t) = flip validNames rules <$> values
  in
    foldl' intersect h t

-- Find all possible names for each field
getConstrains :: [Rule] -> [[Int]] -> [[Text]]
getConstrains rules others =
  flip findNames rules
  <$> transpose (filterInvalid rules others)

-- TODO: We can likely generalise this. For example, we could've found the order
-- using toposort, which would work in more cases than sorting by the number of
-- available options
solveConstrains :: [[Text]] -> [(Int, Text)]
solveConstrains constrains =
  let
    ixedConstrains :: [(Int, [Text], Int)] =
      sort $ zip3 (length <$> constrains) constrains [0..]

    f (decided, sol) (_, candidates, pos) =
      let [name] = filter (`notElem` decided) candidates
      in (name:decided, (pos, name):sol)
    (_, names) = foldl' f ([], []) ixedConstrains
  in
    names

solve2 :: ([Rule], [Int], [[Int]]) -> Int
solve2 (rules, ticket, others) =
  let
    fields = solveConstrains . getConstrains rules $ others
    departureFields = filter (isPrefixOf "departure" . view _2) fields
    indices = view _1 <$> departureFields
  in
    product $ (ticket !!) <$> indices

main :: IO ()
main = do
  input <- getParsedInput day parser

  putStr "Solution 1: "
  print $ solve1 input

  putStr "Solution 2: "
  print $ solve2 input
