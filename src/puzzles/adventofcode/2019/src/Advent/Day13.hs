-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Advent.Day13 where

import           Prelude                   hiding (Left, Right, concat, getLine,
                                            putStrLn, readFile, show)

import           Advent.Day13.GameInternal
import           Advent.Day13.Intcode      hiding (initial_state)
import           Advent.Day13.Internal     hiding (initial_state)
import           Control.Lens              (_1, _2, assign, at, ix, modifying,
                                            set, toListOf, use, uses, view)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.State       (StateT, get, lift, runStateT)
import           Data.Advent               (Day (..))
import           Data.Functor.Identity     (runIdentity)
import           Data.Generics.Labels      ()
import           Data.Map.Strict           (Map, keys)
import           Data.Text                 (Text, concat, intercalate, splitOn,
                                            unpack)
import           Data.Text.IO              (putStrLn)
import           System.Console.ANSI       (clearScreen, setCursorPosition)
import           System.IO.Advent          (getInput)


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

type Coord = (Int, Int)

encode :: (Num c, Enum a) => a -> c
encode = fromIntegral . fromEnum

decode :: (Integral c, Enum a) => c -> a
decode = toEnum . fromIntegral

read_game :: [Integer] -> [Integer]
read_game code =
  view _1 . runIdentity $ run (run_program >> get_output) code

parse_output :: Monad m => [Integer] -> GameT m ()
parse_output (-1 : 0 : score : t) =
  do
    assign #score score
    parse_output t
parse_output (x : y : thing : t) =
  let
    coord = (fromIntegral x, fromIntegral y)
    element = decode thing
  in do
    assign (#screen . at coord) . Just $ element
    parse_output t
parse_output [] = pure ()
parse_output _ = undefined


show_map :: (Maybe a -> Text) -> Map Coord a -> Text
show_map format plane =
  let
    coords = keys plane
    xs = toListOf (traverse . _1) coords
    ys = toListOf (traverse . _2) coords
    max_x = maximum xs
    min_x = minimum xs
    max_y = maximum ys
    min_y = minimum ys
    row y = (, y) <$> [min_x..max_x]
    show_coord coord = format $ view (at coord) plane
    printed y = concat (show_coord <$> row y)
  in
    intercalate "\n" (printed <$> [min_y..max_y])

show_screen :: Map Coord Tile -> Text
show_screen =
  let
    formatter (Just Empty)  = " "
    formatter (Just Wall)   = "#"
    formatter (Just Block)  = "X"
    formatter (Just Paddle) = "="
    formatter (Just Ball)   = "O"
    formatter Nothing       = " "
  in show_map formatter

get_input :: IO [Integer]
get_input = fmap (read . unpack) . splitOn "," <$> getInput D13

get_input_2 :: IO [Integer]
get_input_2 = set (ix 0) 2 <$> get_input

insert_quarter :: [Integer] -> [Integer]
insert_quarter = set (ix 0) 2

type GameT m = StateT GameState (IntcodeT m)

step_game :: Monad m => GameT m Status
step_game =
  do
    lift run_program
    lift get_output >>= parse_output
    lift $ use #status

input_joystick :: Monad m => Integer -> GameT m ()
input_joystick x = modifying #inputs (x:) >> (lift . push_input $ [x])

replay :: Monad m => [Integer] -> GameT m ()
replay = sequence_ . fmap input_joystick

get_saved :: Monad m => GameT m ([Integer], ComputerState)
get_saved = (,) <$> uses #inputs reverse <*> use #saved_state

save :: Monad m => GameT m ComputerState
save = lift get

run_game :: Monad m => GameT m a -> [Integer]-> m (a, GameState, ComputerState, Text)
run_game game code =
  let program = runStateT game initial_state
  in do
    ((a, game_state), computer_state, traces) <- run program code
    pure (a, game_state, computer_state, traces)

run_saved ::Monad m => GameT m a -> ComputerState -> m (a, GameState, ComputerState, Text)
run_saved game st =
  let program = runStateT game initial_state
  in do
    ((a, game_state), computer_state, traces) <- run' program st
    pure (a, game_state, computer_state, traces)

play :: GameT IO ()
play =
  do
    status <- step_game
    if status == Interrupted then
      do
        input <- get_joystick
        if input == 2
          then save >>= assign #saved_state
          else do
          input_joystick input
          play
      else
      do
        liftIO . putStrLn $ "Game Over (" <> show status <> ")"
        pure ()

get_joystick :: GameT IO Integer
get_joystick = do
  screen <- use #screen
  score <- use #score
  liftIO $ do
    clearScreen
    setCursorPosition 0 0
    putStrLn $ "Score: " <> show score
    putStrLn $ show_screen screen
    putStr ">"
  liftIO getChar >>= \case
    'j' -> pure (-1)
    'k' -> pure 0
    'l' -> pure 1
    's' -> pure 2
    _ ->
      do
        liftIO . putStrLn $ "Invalid joystick value"
        get_joystick

main :: IO ()
main = do
  -- code <- get_input
  -- putStrLn $ "Solution 1: " <> show (size . fromList . parse_output . read_game $ code)
  putStrLn "No automatic solutions for this one (yet) :("
