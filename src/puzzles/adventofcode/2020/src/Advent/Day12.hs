{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day12 where

import           Advent.Perlude

import           Control.Lens             (at, both, each, foldlOf, over,
                                           preview, view, _2, _head, _tail)
import           Control.Monad            (guard)
import           Control.Monad.State.Lazy (MonadState)
import           Data.Generics.Labels     ()
import           Data.List                (find, foldl', sort, unfoldr)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe               (fromJust, isJust)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import           Data.Text.Lens           (packed, unpacked)
import qualified System.IO.Advent         as IOAdvent
import qualified Text.Read                as Read

import           Advent.Day12.Internal    (Action (..), Direction (..),
                                           Instruction, Ship, mkInstruction,
                                           mkShip)

-- TODO: This is part of the most recent base (for String), make it for Text in
-- our prelude
readMaybe :: Read a => Text -> Maybe a
readMaybe = Read.readMaybe . unpack

example :: Text
example = "F10\n\
          \N3\n\
          \F7\n\
          \R90\n\
          \F11\n"

getInput :: IO Text
getInput = IOAdvent.getInput "12"

parseInstruction :: Text -> Maybe Instruction
parseInstruction textInstruction = do
  action <- preview (unpacked . _head) textInstruction >>= parseAction
  amount <- preview (unpacked . _tail . packed) textInstruction >>= readMaybe
  pure $ mkInstruction action amount

parseAction :: Char -> Maybe Action
parseAction 'F' = Just F
parseAction 'R' = Just $ T True
parseAction 'L' = Just $ T False
parseAction 'N' = Just $ M N
parseAction 'S' = Just $ M S
parseAction 'E' = Just $ M E
parseAction 'W' = Just $ M W
parseAction _   = Nothing

parse :: Text -> Maybe [Instruction]
parse = traverse parseInstruction . Text.lines

main :: IO ()
main = do
  input <- getInput

  putStr "Solution 1: "
  print $ "NA"

  putStr "Solution 2: "
  print $ "NA"
