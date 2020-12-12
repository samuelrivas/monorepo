{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day12 where

import           Advent.Perlude

import           Control.Lens             (at, both, each, foldlOf, modifying,
                                           over, preview, use, view, _2, _head,
                                           _tail)
import           Control.Monad            (guard)
import           Control.Monad.State.Lazy (MonadState, execState)
import           Data.Bidim               (Coord, plus)
import           Data.Foldable            (traverse_)
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
  validateInstruction action amount
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

validateInstruction :: Action -> Int -> Maybe ()
validateInstruction (T _) 0   = Just ()
validateInstruction (T _) 90  = Just ()
validateInstruction (T _) 180 = Just ()
validateInstruction (T _) 270 = Just ()
validateInstruction (T _) _   = Nothing
validateInstruction _ _       = Just ()

parse :: Text -> Maybe [Instruction]
parse = traverse parseInstruction . Text.lines

step :: MonadState Ship m => Instruction -> m ()
step instruction =
  let
    amount = view #amount instruction
  in
    case view #action instruction of
      F           -> moveForward amount
      T clockwise -> turn clockwise amount
      M direction -> move direction amount

moveForward :: MonadState Ship m => Int -> m ()
moveForward amount = do
  direction <- use #direction
  modifying #position $ newPosition direction amount

turn :: MonadState Ship m => Bool -> Int -> m ()
turn isClockwise amount = modifying #direction $ newDirection isClockwise amount

move :: MonadState Ship m => Direction -> Int -> m ()
move direction = modifying #position . newPosition direction

-- TODO: Something like this would live in Bidim
newPosition :: Direction -> Int -> Coord -> Coord
newPosition direction amount position =
  position `plus`
  case direction of
    N -> (0, amount)
    S -> (0, -amount)
    W -> (amount, 0)
    E -> (-amount, 0)

-- TODO: Something like this would live in Bidim
newDirection :: Bool -> Int -> Direction -> Direction
newDirection True degrees  = toEnum . (`mod` 4) . (toSteps degrees +) . fromEnum
newDirection False degrees = toEnum . (`mod` 4) . ((- toSteps degrees) + ) . fromEnum

 -- TODO: Avoid the undefined using a proper type instead of Int
toSteps :: Int -> Int
toSteps 90  = 1
toSteps 180 = 2
toSteps 270 = 3
toSteps _   = undefined

solution1 :: [Instruction] -> Int
solution1 instructions =
  let
    endState = execState (traverse_ step instructions) mkShip
    (x, y) = view #position endState
  in
    abs x + abs y

main :: IO ()
main = do
  input <- fromJust . parse <$> getInput

  putStr "Solution 1: "
  print $ solution1 input

  putStr "Solution 2: "
  print $ "NA"
