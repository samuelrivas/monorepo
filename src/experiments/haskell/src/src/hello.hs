-- TODO: Consider make the board a state monad

import           Control.Monad
import qualified Data.List     as List
import qualified Data.Map.Lazy as Map
import           Data.Maybe
import qualified Data.Set      as Set

data Pos = A | B | C
         deriving (Eq, Show, Bounded, Enum, Ord)

newtype Column = Column Pos deriving (Eq, Ord)
newtype Row = Row Pos deriving (Eq, Ord)

instance Show Column where
  show (Column x) = "Column(" ++ (show x) ++ ")"

instance Show Row where
  show (Row x) = "Row(" ++ (show x) ++ ")"

newtype Coordinate = Coordinate (Row, Column) deriving (Eq, Ord, Show)

data Player = X | O deriving (Eq, Ord, Show)
newtype Cell = Cell (Maybe Player) deriving (Eq, Ord)

instance Show Cell where
  show = show_cell

newtype Board = Board {get_map :: Map.Map Coordinate Cell}
instance Show Board where
  show = show_board

coord :: (Pos, Pos) -> Coordinate
coord (x, y) = Coordinate(Row x, Column y)

empty_board :: Board
empty_board =
  let positions = [A .. C]
      coordinates = [coord (x, y)
                    | x <- positions
                    , y <- positions]
      add = \m c -> Board (Map.insert c (Cell Nothing) (get_map m))
  in foldl add (Board Map.empty) coordinates

show_cell :: Cell -> String
show_cell (Cell (Just x)) = show x
show_cell (Cell Nothing) = " "

show_row :: Board -> Row -> String
show_row board (Row row) =
 let coords = [coord (row, col) | col <- [A .. C]]
     cells = [get_map board Map.! c | c <- coords]
 in "| " ++ (List.intercalate " | " $ map show cells) ++ " |"

show_board :: Board -> String
show_board board =
  let rows = [Row pos | pos <- [A .. C]]
      shown_rows = map (show_row board) rows
      sep = "+---+---+---+"
  in unlines $ [sep] ++ (List.intersperse sep shown_rows) ++ [sep]

set_cell :: Board -> Player -> Coordinate -> Board
set_cell board player coordinate =
  let m = get_map board
      cell = Cell $ Just player
  in Board (Map.insert coordinate cell m)

get_cell :: Board -> Coordinate -> Cell
get_cell board coordinate =
  get_map board Map.! coordinate

all_lines :: Board -> Set.Set [Cell]
all_lines board =
   let map = get_map board
       rows = [[coord (r, A), coord (r, B), coord (r, C)] | r <- [A .. C]]
       cols = [[coord (A, c), coord (B, c), coord (C, c)] | c <- [A .. C]]
       diag = [[coord (A, A), coord (B, B), coord (C, C)],
               [coord (A, C), coord (B, B), coord (C, A)]]
   in Set.fromList $ List.map (List.map (get_cell board)) $ concat [rows, cols, diag]

winner :: Board -> Maybe Player
winner board =
  let is_winner x = Set.member (replicate 3 (Cell $ Just x)) $ all_lines board
  in List.find is_winner [X, O]

parse_coordinate :: String -> Maybe Coordinate
parse_coordinate [first, second] =
  do
    x <- parse_position first
    y <- parse_position second
    return $ coord(x, y)
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
get_coordinate = parse_coordinate <$> getLine

repeat_until_just :: IO (Maybe a) -> IO a
repeat_until_just action =
  action >>= maybe (repeat_until_just action) return

turn :: Board -> Player -> IO Board
turn board player = do
  putStrLn $ show board
  putStr "> "
  coordinate <- repeat_until_just get_coordinate
  return $ set_cell board player coordinate

game_loop :: Board -> Player -> IO ()
game_loop board player =
  when (isNothing $ winner board) $ do
    newBoard <- turn board player
    game_loop newBoard (swap_player player)

main :: IO ()
main = game_loop empty_board X
