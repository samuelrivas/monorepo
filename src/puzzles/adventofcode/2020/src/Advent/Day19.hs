{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day19 where

import           Advent.Perlude

import           Control.Lens         (at, both, each, foldlOf, over, view, _2)
import           Control.Monad        (guard)
import           Data.Foldable        (fold)
import           Data.List            (find, foldl', sort, unfoldr)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe           (fromJust, isJust)
import           Data.Set             (Set)
import qualified Data.Set             as Set
import qualified Data.Text            as Text
import qualified System.IO.Advent     as IOAdvent
import           Text.Regex.TDFA      ((=~))
import           Text.Regex.TDFA.Text ()

getInput :: IO Text
getInput = IOAdvent.getInput "19"

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

data Rule = Refs [[Int]]
    | Literal Char
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

ruleToRegexText :: Grammar -> Rule -> Text
ruleToRegexText _ (Literal c) = Text.singleton c
ruleToRegexText grammar (Refs refs) = Text.intercalate "|" $ reduceRef grammar <$> refs

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

main :: IO ()
main = do
  input <- getInput

  putStr "Solution 1: "
  print $ "NA"

  putStr "Solution 2: "
  print $ "NA"

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
