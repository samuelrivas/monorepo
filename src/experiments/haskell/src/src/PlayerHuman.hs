module PlayerHuman
  ( mk_player
  ) where

import qualified Board
import qualified GameState
import qualified IOUtils
import qualified Player
import           System.IO

{-# ANN module "HLint: ignore Use camelCase" #-}

mk_player :: Player.Player
mk_player = Player.mk_player next_move

parse_coordinate :: String -> Maybe Board.Coordinate
parse_coordinate [first, second] =
  do
    x <- parse_position first
    y <- parse_position second
    return $ Board.mk_coordinate(x, y)
parse_coordinate _ = Nothing

parse_position :: Char -> Maybe Board.Pos
parse_position 'A' = Just Board.A
parse_position 'B' = Just Board.B
parse_position 'C' = Just Board.C
parse_position _ = Nothing

get_coordinate :: IO (Maybe Board.Coordinate)
get_coordinate = parse_coordinate <$> prompt_line

prompt_line :: IO String
prompt_line = putStr "> " >> hFlush stdout >> getLine

mk_legal_move :: GameState.State -> Board.Coordinate -> Maybe GameState.Move
mk_legal_move state coordinate =
  let
    player = GameState.get_control_player state
    move = GameState.mk_move player coordinate
  in
    if GameState.legal_move state move
    then Just move
    else Nothing

get_move :: GameState.State -> IO (Maybe GameState.Move)
get_move state =
  let mk_move maybe_coordinate = do
        coordinate <- maybe_coordinate
        mk_legal_move state coordinate
  in do
    print state
    coordinate <- get_coordinate
    return $ mk_move coordinate

next_move :: GameState.State -> IO GameState.Move
next_move state = IOUtils.repeat_until_just (get_move state)
