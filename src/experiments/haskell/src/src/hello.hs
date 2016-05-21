-- TODO: Consider make the board a state monad

import           Board
import           Control.Monad
import           Data.Maybe
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

swap_player :: Player -> Player
swap_player X = O
swap_player O = X

get_coordinate :: IO (Maybe Coordinate)
get_coordinate = parse_coordinate <$> prompt_line

prompt_line :: IO String
prompt_line = putStr "> " >> hFlush stdout >> getLine

repeat_until_just :: IO (Maybe a) -> IO a
repeat_until_just action =
  action >>= maybe (repeat_until_just action) return

turn :: Board -> Player -> IO Board
turn board player = do
  putStrLn $ show board
  coordinate <- repeat_until_just get_coordinate
  return $ set_cell board player coordinate

game_loop :: Board -> Player -> IO ()
game_loop board player =
  when (isNothing $ winner board) $ do
    newBoard <- turn board player
    game_loop newBoard (swap_player player)

main :: IO ()
main = game_loop empty_board X
