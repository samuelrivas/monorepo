{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day16 where

import           Advent.Perlude

import           Control.Lens              (at, both, each, foldlOf, over,
                                            traverse, view, _2)
import           Control.Monad.Fail        (MonadFail)
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.Maybe
import           Data.List                 (find, foldl', sort, unfoldr)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromJust, isJust)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import qualified Data.Text                 as Text
import qualified System.IO.Advent          as IOAdvent
import qualified Text.Read                 as Read

import           Advent.Day16.Internal     (Rule, mkRule)

-- TODO: This is part of the most recent base (for String), make it for Text in
-- our prelude
readMaybe :: MonadFail m => Read a => Text -> m a
readMaybe = liftMaybe . Read.readMaybe . unpack

-- doesn't this already exist?
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

parse :: MonadFail m => Text -> m ([Rule], [Int], [[Int]])
parse input = do
  [rulesText, ticketText, othersText]
    <- pure . fmap Text.strip . Text.splitOn "\n\n" $ input

  rules <- parseRules rulesText
  ticket <- parseTicket ticketText
  others <- parseOthers othersText
  pure (rules, ticket, others)

parseRules :: MonadFail m => Text -> m [Rule]
parseRules = traverse parseRule . Text.lines

parseTicket :: MonadFail m => Text -> m [Int]
parseTicket text = liftMaybe $ do
  textNumbers <- Text.stripPrefix "your ticket:\n" text
  traverse readMaybe $ Text.splitOn "," textNumbers

parseOthers :: MonadFail m => Text -> m [[Int]]
parseOthers text = liftMaybe $ do
    textTickets <- Text.stripPrefix "nearby tickets:\n" text
    traverse (traverse readMaybe . Text.splitOn ",") $ Text.splitOn "\n" textTickets

getInput :: IO Text
getInput = IOAdvent.getInput "16"

parseRule :: MonadFail m => Text -> m Rule
parseRule text = do
  [name, ranges] <- pure . Text.splitOn ": " $ text
  [l, h] <- pure . Text.splitOn " or " $ ranges
  mkRule name <$> parseRange l <*> parseRange h

parseRange :: MonadFail m => Text -> m (Int, Int)
parseRange text = do
  [lo, hi] <- pure . fmap read . Text.splitOn "-" $ text
  pure (lo, hi)

main :: IO ()
main = do
  input <- getInput

  putStr "Solution 1: "
  print $ "NA"

  putStr "Solution 2: "
  print $ "NA"
