{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Advent.Day12 where

import           Advent.Perlude

import           Control.Lens             (both, modifying, over, use, view)
import           Control.Monad.State.Lazy (MonadState, execState)
import           Data.Bidim               (Coord, plus)
import           Data.Foldable            (traverse_)
import           Data.Functor             (($>))
import           Data.Generics.Labels     ()
import           Text.Parsec              (char, sepEndBy, unexpected, (<|>))

import           Advent.Day12.Internal    (Action (..), Direction (..),
                                           Instruction, Ship, Ship2,
                                           mkInstruction, mkShip, mkShip2)
import           Advent.Templib           (Day (..), getInput', getParsedInput)
import           Advent.Templib.Parsec    (Parser, digitsAsNum)

day :: Day
day = D12

example :: Text
example = "F10\n\
          \N3\n\
          \F7\n\
          \R90\n\
          \F11\n"

getInput :: IO Text
getInput = getInput' D12

parseTurn :: Parser Instruction
parseTurn =
  let action = char 'R' $> T True <|> char 'L' $> T False
  in mkInstruction <$> action <*> parseDegrees

parseMove :: Parser Instruction
parseMove =
  let action =
        char 'F' $> F
        <|> char 'N' $> M N
        <|> char 'S' $> M S
        <|> char 'E' $> M E
        <|> char 'W' $> M W
  in
    mkInstruction <$> action <*> digitsAsNum

parseDegrees :: Parser Int
parseDegrees = do
  n <- digitsAsNum
  if n `elem` [0, 90, 180, 270]
    then pure n
    else unexpected "invalid degree amount in turn"

parser :: Parser [Instruction]
parser = (parseMove <|> parseTurn) `sepEndBy` char '\n'

step :: MonadState Ship m => Instruction -> m ()
step instruction =
  let amount = view #amount instruction
  in case view #action instruction of
      F           -> moveForward amount
      T clockwise -> turn clockwise amount
      M direction -> move direction amount
step2 :: MonadState Ship2 m => Instruction -> m ()
step2 instruction =
  let amount = view #amount instruction
  in case view #action instruction of
    F           -> moveToViewpoint amount
    T clockwise -> turnViewpoint clockwise amount
    M direction -> moveViewpoint direction amount

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
    W -> (-amount, 0)
    E -> (amount, 0)

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

moveToViewpoint :: MonadState Ship2 m => Int -> m ()
moveToViewpoint amount = do
  viewpoint <- use #viewpoint
  modifying #position $ plus (over both (* amount) viewpoint)

turnViewpoint :: MonadState Ship2 m => Bool -> Int -> m ()
turnViewpoint clockwise = modifying #viewpoint . rotateCoord clockwise

-- TODO: Avoid the undefined using a proper type instead of Int
-- TODO: generalise this in Bidim
rotateCoord :: Bool -> Int -> Coord -> Coord
rotateCoord True 90  = rotate90
rotateCoord True 180 = rotate90 . rotate90
rotateCoord True 270 = rotate90 . rotate90 . rotate90
rotateCoord False n  = rotateCoord True (360 - n)
rotateCoord _ _      = undefined

moveViewpoint :: MonadState Ship2 m => Direction -> Int -> m ()
moveViewpoint direction = modifying #viewpoint . newPosition direction

-- TODO: Generalise this in bidim. Rotating coord (x, y) counterclockwise is
--
-- [cos -sin  [x
--  sin  cos]  y]
rotate90 :: Coord -> Coord
rotate90 (x, y) = (y, -x)

solution1 :: [Instruction] -> Int
solution1 instructions =
  let
    endState = execState (traverse_ step instructions) mkShip
    (x, y) = view #position endState
  in
    abs x + abs y

solution2 :: [Instruction] -> Int
solution2 instructions =
  let
    endState = execState (traverse_ step2 instructions) mkShip2
    (x, y) = view #position endState
  in
    abs x + abs y

main :: IO ()
main = do
  input <- getParsedInput day parser

  putStr "Solution 1: "
  print $ solution1 input

  putStr "Solution 2: "
  print $ solution2 input
