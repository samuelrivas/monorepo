-- TODO: Consider make the board a state monad

import           Board
import qualified Data.Map.Lazy as Map
import qualified GameState     as GameState
import           System.IO

parse_coordinate :: String -> Maybe Board.Coordinate
parse_coordinate [first, second] =
  do
    x <- parse_position first
    y <- parse_position second
    return $ from_ints(x, y)
parse_coordinate _ = Nothing

parse_position :: Char -> Maybe Pos
parse_position 'A' = Just A
parse_position 'B' = Just B
parse_position 'C' = Just C
parse_position _ = Nothing

get_coordinate :: IO (Maybe Coordinate)
get_coordinate = parse_coordinate <$> prompt_line

prompt_line :: IO String
prompt_line = putStr "> " >> hFlush stdout >> getLine

repeat_until_just :: IO (Maybe a) -> IO a
repeat_until_just action =
  action >>= maybe (repeat_until_just action) return

read_move_and_update :: GameState.State -> IO (Maybe GameState.State)
read_move_and_update state =
  let update_state maybe_coordinate = do
        coordinate <- maybe_coordinate
        move <- GameState.mk_move state coordinate
        GameState.next_state state move
  in do
    coordinate <- get_coordinate
    return $ update_state coordinate

turn :: GameState.State -> IO GameState.State
turn state = do
  putStrLn $ show state
  repeat_until_just $ read_move_and_update state

game_loop :: GameState.State -> IO GameState.State
game_loop state =
  let more_moves = (length $ GameState.possible_moves state) > 0
  in
    if (more_moves)
    then do
      new_state <- turn state
      game_loop new_state
    else
      return state

show_winner :: GameState.State -> IO ()
show_winner state =
  let scores = Map.toList $ GameState.scores state
      show_score (player, score) = show player ++ ": " ++ show score
      to_print_lines = map show_score scores
  in
    mapM_ putStrLn to_print_lines

main :: IO ()
main = do
  end_state <- game_loop GameState.initial_state
  putStrLn $ show end_state
  show_winner end_state
