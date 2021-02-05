{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day19 where

import           Advent.Perlude

import           Control.Lens          (at, over, set, view, _1, _2)
import           Data.Foldable         (fold)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (fromJust)
import qualified Data.Text             as Text
import           Text.Parsec           (between, char, oneOf, sepBy, sepEndBy,
                                        (<|>))
import           Text.Regex.TDFA       ((=~))
import           Text.Regex.TDFA.Text  ()

import           Advent.Templib        (Day (..), getInput', getParsedInput)
import           Advent.Templib.Parsec (Parser, digitsAsNum, literal, text1)

day :: Day
day = D19

getInput :: IO Text
getInput = getInput' D19

example :: Text
example = "0: 4 1 5\n\
          \1: 2 3 | 3 2\n\
          \2: 4 4 | 5 5\n\
          \3: 4 5 | 5 4\n\
          \4: \"a\"\n\
          \5: \"b\"\n\
          \\n\
          \ababbb\n\
          \bababa\n\
          \abbbab\n\
          \aaabbb\n\
          \aaaabbb\n"

longExample :: Text
longExample = "42: 9 14 | 10 1\n\
              \9: 14 27 | 1 26\n\
              \10: 23 14 | 28 1\n\
              \1: \"a\"\n\
              \11: 42 31\n\
              \5: 1 14 | 15 1\n\
              \19: 14 1 | 14 14\n\
              \12: 24 14 | 19 1\n\
              \16: 15 1 | 14 14\n\
              \31: 14 17 | 1 13\n\
              \6: 14 14 | 1 14\n\
              \2: 1 24 | 14 4\n\
              \0: 8 11\n\
              \13: 14 3 | 1 12\n\
              \15: 1 | 14\n\
              \17: 14 2 | 1 7\n\
              \23: 25 1 | 22 14\n\
              \28: 16 1\n\
              \4: 1 1\n\
              \20: 14 14 | 1 15\n\
              \3: 5 14 | 16 1\n\
              \27: 1 6 | 14 18\n\
              \14: \"b\"\n\
              \21: 14 1 | 1 14\n\
              \25: 1 1 | 1 14\n\
              \22: 14 14\n\
              \8: 42\n\
              \26: 14 22 | 1 20\n\
              \18: 15 15\n\
              \7: 14 5 | 1 21\n\
              \24: 14 1\n\
              \\n\
              \abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\n\
              \bbabbbbaabaabba\n\
              \babbbbaabbbbbabbbbbbaabaaabaaa\n\
              \aaabbbbbbaaaabaababaabababbabaaabbababababaaa\n\
              \bbbbbbbaaaabbbbaaabbabaaa\n\
              \bbbababbbbaaaaaaaabbababaaababaabab\n\
              \ababaaaaaabaaab\n\
              \ababaaaaabbbaba\n\
              \baabbaaaabbaaaababbaababb\n\
              \abbbbabbbbaaaababbbbbbaaaababb\n\
              \aaaaabbaabaaaaababaa\n\
              \aaaabbaaaabbaaa\n\
              \aaaabbaabbaaaaaaabbbabbbaaabbaabaaa\n\
              \babaaabbbaaabaababbaabababaaab\n\
              \aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba\n"

data Rule = Refs [[Int]]
    | Literal Char
    | Rule8
    | Rule11
    deriving stock (Show, Eq)

type Grammar = Map Int Rule

exampleGrammar :: Grammar
exampleGrammar = Map.fromList [
  (0, Refs [[4, 1, 5]]),
  (1, Refs [[2, 3], [3, 2]]),
  (2, Refs [[4, 4], [5, 5]]),
  (3, Refs [[4, 5], [5, 4]]),
  (4, Literal 'a'),
  (5, Literal 'b')
  ]

parser :: Parser (Grammar, [Text])
parser =
      (,)
  <$> parseGrammar <* char '\n'
  <*> parseMatches

parseGrammar :: Parser Grammar
parseGrammar = Map.fromList <$> parseRule `sepEndBy` char '\n'

parseRule :: Parser (Int, Rule)
parseRule =
  (,)
  <$> digitsAsNum <* literal ": "
  <*> parseRuleBody

parseRuleBody :: Parser Rule
parseRuleBody =
      Literal <$> between (char '"') (char '"') (oneOf "ab")
  <|> Refs <$> parseRefs

parseRefs :: Parser [[Int]]
parseRefs =
  let p = digitsAsNum `sepEndBy` char ' '
  in p `sepBy` literal "| "

parseMatches :: Parser [Text]
parseMatches =  text1 (oneOf "ab") `sepEndBy` char '\n'

ruleToRegexText :: Grammar -> Rule -> Text
ruleToRegexText _ (Literal c) = Text.singleton c
ruleToRegexText grammar (Refs refs) = Text.intercalate "|" $ reduceRef grammar <$> refs
ruleToRegexText grammar Rule8 = reduceRef grammar [42] <> "+"
-- Rule 11 is not a regex, but since it always goes after rule 8, we can remove
-- its first part and make it regexable
ruleToRegexText grammar Rule11 = reduceRef grammar [31] <> "+"

reduceRef :: Grammar -> [Int] -> Text
reduceRef grammar ref =
  let
    expand n = fromJust . view (at n) $ grammar
    rules = expand <$> ref
    regexes = ruleToRegexText grammar <$> rules
    parenthesise t = "(" <> t <> ")"
  in
    Data.Foldable.fold $ parenthesise <$> regexes

grammarToRegex :: Grammar -> Text
grammarToRegex grammar = "^" <> ruleToRegexText grammar (Refs [[0]]) <> "$"

isMatch :: Grammar -> Text -> Bool
isMatch grammar = (=~ grammarToRegex grammar)

solution1 :: (Grammar, [Text]) -> Int
solution1 (grammar, matches) = sum . fmap fromEnum $ isMatch grammar <$> matches

-- This is wrong, the solution matches something like (rule 42)* (rule 31)*,
-- where there are more applications of 41 than 31. But this doesn't obey this
-- latest restriction
solution2 :: (Grammar, [Text]) -> Int
solution2 (grammar, matches) =
  sum . fmap fromEnum $ isMatch (modifyGrammar grammar) <$> matches

-- TODO: This is just a dirty copy from what I did in the repl. Clean this up
solution2' :: (Grammar, [Text]) -> Int
solution2' (g, m) =
  let
    g' = modifyGrammar g
    matches = filter (isMatch g') m
    r8 = ("^" <>) . ruleToRegexText g $ Refs [[42]]
    r11 = ("^" <>) . ruleToRegexText g $ Refs [[31]]
    pass1 = consumeRegex r8 <$> matches
    pass2 = over _2 (consumeRegex r11) <$> pass1
  in
    length . filter (\(x, (y, _)) -> x > y) $ pass2

modifyGrammar :: Grammar -> Grammar
modifyGrammar = set (at 8) (Just Rule8) . set (at 11) (Just Rule11)

consumeRegex :: Text -> Text -> (Int, Text)
consumeRegex regex text =
  let
    (left, match, right) :: (Text, Text, Text) = text =~ regex
  in
    if Text.null match then
      (0, text)
    else
      over _1 (+1) $ consumeRegex regex (left <> right)

main :: IO ()
main = do
  input <- getParsedInput day parser

  putStr "Solution 1: "
  print . solution1 $ input

  putStr "Solution 2: "
  print . solution2' $ input

-- TODO: This took quite a while to figure out, the intuition just came when I
-- thought of it as concatenating a list of Maybes.
--
-- We want to turn [["a", "b"], ["1", "2"]] into ["a1", "a2", "b1",
-- "b2"]. Thinking of lists as a monad, we need to turn m [Text] into m Text. If
-- whe change the monad to Maybe, that is turning [Just "a", Just "b"] into Just
-- "ab".
--
-- What makes it confusing with the list monad is that we are using lists in two
-- different ways. The outer list is a Traversable, whereas the inner list is
-- the monad. In the example, the inner moand is Maybe, which makes the intent
-- more clear.
--
-- So we use sequence to t m into m t. Then we use fold to remove the
-- traversable, using the monoid instance of Text.
--
-- Try to find a better way to make this digestable. Or at the very least make
-- some notes for the future: sequence swaps t (m a) to m (t a), fold removes
-- the t from t a to a. And is important to know what you are traversing and
-- what you are using as monad when dealing with nested structures.
--
-- This was fun, but is not going to work with the big input so ...
-- reduceRef :: Grammar -> [Int] -> [Text]
-- reduceRef grammar ref =
--   let
--     expand n = fromJust . view (at n) $ grammar
--     rules = expand <$> ref
--     reduced :: [[Text]] = reduceRule grammar <$> rules
--   in
--     fmap Data.Foldable.fold . sequence $ reduced
