-- TODO: Consider make the board a state monad

import qualified Board
import qualified Data.Map.Lazy as Map
import qualified GameState
import           System.IO

{-# ANN module "HLint: ignore Use camelCase" #-}

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

repeat_until_just :: IO (Maybe a) -> IO a
repeat_until_just action =
  action >>= maybe (repeat_until_just action) return

mk_legal_move :: GameState.State -> Board.Coordinate -> Maybe GameState.Move
mk_legal_move state coordinate =
  let
    player = GameState.get_control_player state
    move = GameState.mk_move player coordinate
  in
    if GameState.legal_move state move
    then Just move
    else Nothing

read_move_and_update :: GameState.State -> IO (Maybe GameState.State)
read_move_and_update state =
  let update_state maybe_coordinate = do
        coordinate <- maybe_coordinate
        move <- mk_legal_move state coordinate
        GameState.next_state state move
  in do
    coordinate <- get_coordinate
    return $ update_state coordinate

turn :: GameState.State -> IO GameState.State
turn state = do
  print state
  repeat_until_just $ read_move_and_update state

game_loop :: GameState.State -> IO GameState.State
game_loop state =
  let more_moves = null $ GameState.possible_moves state
  in
    if more_moves
    then do
      new_state <- turn state
      game_loop new_state
    else
      return state

show_winner :: GameState.State -> IO ()
show_winner state =
  let scores = Map.toList $ GameState.scores state

      show_score :: (GameState.Player, Integer) -> String
      show_score (player, score) = show player ++ ": " ++ show score

      to_print_lines = map show_score scores
  in
    mapM_ putStrLn to_print_lines

main :: IO ()
main = do
  end_state <- game_loop GameState.initial_state
  print end_state
  show_winner end_state
