{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Advent.Day10 where

import           Perlude

import           Advent.Templib       (linesOf)

import           Control.Lens         (_1, _2, _head, preview, view)
import           Control.Monad.Loops  (dropWhileM)
import           Control.Monad.State  (MonadState, get, modify, put, runState)
import           Data.Advent          (Day (..))
import           Data.Functor         (($>))
import           Data.Generics.Labels ()
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HashSet
import           Data.List            (sort)
import           Data.Maybe           (fromJust, isNothing, mapMaybe)
import           Data.Text            (intercalate)
import           System.IO.Advent     (getInput, solve)
import           Text.Parsec          (oneOf)
import           Text.Parsec.Parselib (Parser, text1, unsafeParseAll)

-- TODO This problem may be solvable just using parsec
type Parsed =  [Text]

day :: Day
day = D10

rawInput :: IO Text
rawInput = getInput day

example :: Text
example =
  intercalate
    "\n"
    [ "[({(<(())[]>[[{[]{<()<>>",
      "[(()[<>])]({[<{<<[]>>(",
      "{([(<{}[<>[]}>{[]{[(<()>",
      "(((({<>}<{<{<>}{[]{[]{}",
      "[[<[([]))<([[{}[[()]]]",
      "[{[{({}]{}}([{[{{{}}([]",
      "{<[[]]>}<{[{[{[]{()[[[]",
      "[<(<(<(<{}))><([]([]()",
      "<{([([[(<>()){}]>(<<{{",
      "<{([{{}}[<[[[<>{}]]]>[]]"
    ]

parsedExample :: Parsed
parsedExample = fromJust $ unsafeParseAll parser example

parser :: Parser Parsed
parser = linesOf $ text1 (oneOf "[](){}<>")

opening :: HashSet Char
opening = HashSet.fromList "([{<"

closing :: HashSet Char
closing = HashSet.fromList ")]}>"

match :: Char -> Char -> Bool
match '(' ')' = True
match '[' ']' = True
match '{' '}' = True
match '<' '>' = True
match _ _     = False

-- Returns false if corrupted
pushSymbol :: MonadState [Char] m => Char -> m Bool
pushSymbol c =
  if HashSet.member c opening then modify (c:) $> True
  else
    get >>= \case
    [] -> pure False
    (h:t) ->
      if match h c then put t $> True
      else pure False

-- Consumes all valid input and returns the unparsed text
parseSyntax :: MonadState [Char] m => Text -> m Text
parseSyntax = fmap pack . dropWhileM pushSymbol . unpack

-- Returns the unconsumed input and the stack. If there is unconsumed input, the
-- parser has failed
runParser :: Text -> (Text, [Char])
runParser = flip runState [] . parseSyntax

illegalChar :: Text -> Maybe Char
illegalChar = preview (_1 . _head) . runParser

isValid :: Text -> Bool
isValid = isNothing . illegalChar

errorScore :: Char -> Int
errorScore ')' = 3
errorScore ']' = 57
errorScore '}' = 1197
errorScore '>' = 25137
errorScore _   = undefined

-- flipped because they stay open in the stack
closeScore :: Char -> Int
closeScore '(' = 1
closeScore '[' = 2
closeScore '{' = 3
closeScore '<' = 4
closeScore _   = undefined

-- It is not flipped, so if we needed to write "))]" this would output "((["
autocompleteSequence :: Text -> [Char]
autocompleteSequence = view _2 . runParser

autocompleteScore :: [Char] -> Int
autocompleteScore = foldl' (\acc c -> 5*acc + closeScore c) 0

findMedian :: [Int] -> Int
findMedian l = sort l !! (length l `div` 2)

solver1 :: Parsed -> Int
solver1 = sum . fmap errorScore . mapMaybe illegalChar

solver2 :: Parsed -> Int
solver2 = findMedian . fmap (autocompleteScore . autocompleteSequence) . filter isValid

main :: IO ()
main = solve day parser solver1 solver2
