{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day22 where

import           Advent.Perlude

import           Control.Lens              (at, both, each, foldlOf, modifying,
                                            non, over, preuse, use, view, _1,
                                            _2, _head)
import           Control.Monad             (guard)
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Generics.Labels      ()
import           Data.List                 (find, foldl', sort, unfoldr)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromJust, isJust)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import qualified Data.Text                 as Text
import qualified System.IO.Advent          as IOAdvent

import           Advent.Day22.Internal

example :: Text
example =
  "Player 1:\n\
  \9\n\
  \2\n\
  \6\n\
  \3\n\
  \1\n\
  \\n\
  \Player 2:\n\
  \5\n\
  \8\n\
  \4\n\
  \7\n\
  \10\n"

getInput :: IO Text
getInput = IOAdvent.getInput "22"

parse :: Text -> Game
parse text =
  let (p1, p2) = over both Text.strip $ Text.breakOn "\n\n" text
  in Game (parsePlayer p1) (parsePlayer p2)

parsePlayer :: Text -> [Int]
parsePlayer = fmap read . tail . Text.lines

-- Returns true if the game ends
-- TODO: This may be more elegant with MaybeT
step :: MonadState Game m => m Bool
step = do
  h1M <- preuse $ #deck1 . _head
  h2M <- preuse $ #deck2 . _head
  case (,) <$> h1M <*> h2M of
    Nothing -> pure True
    Just (h1, h2)
      | h1 > h2 -> do
          modifying #deck2 tail
          modifying #deck1 (++ [h2])
          pure False
      | otherwise -> do
          modifying #deck1 tail
          modifying #deck2 (++ [h1])
          pure False

main :: IO ()
main = do
  input <- getInput

  putStr "Solution 1: "
  print $ "NA"

  putStr "Solution 2: "
  print $ "NA"
