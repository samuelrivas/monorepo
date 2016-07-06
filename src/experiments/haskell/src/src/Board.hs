module Board
  ( Board
  , Coordinate
  , Player(..)
  , Pos(..)
  , empty_board
  , empty_cells
  , get_cell_player
  , mk_coordinate
  , set_cell
  , show_board
  , winner
  ) where

import qualified Data.List     as List
import qualified Data.Map.Lazy as Map
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

mk_coordinate :: (Pos, Pos) -> Coordinate
mk_coordinate (x, y) = Coordinate(Row x, Column y)

empty_board :: Board
empty_board =
  let positions = [A .. C]
      coordinates = [mk_coordinate (x, y)
                    | x <- positions
                    , y <- positions]
      add = \m c -> Board (Map.insert c (Cell Nothing) (get_map m))
  in foldl add (Board Map.empty) coordinates

show_cell :: Cell -> String
show_cell (Cell (Just x)) = show x
show_cell (Cell Nothing) = " "

show_row :: Board -> Row -> String
show_row board (Row row) =
 let coords = [mk_coordinate (row, col) | col <- [A .. C]]
     cells = [get_map board Map.! c | c <- coords]
 in "| " ++ (List.intercalate " | " $ map show cells) ++ " |"

show_board :: Board -> String
show_board board =
  let rows = [Row pos | pos <- [A .. C]]
      shown_rows = map (show_row board) rows
      sep = "+---+---+---+"
  in unlines $ ["", sep] ++ (List.intersperse sep shown_rows) ++ [sep]

set_cell :: Board -> Player -> Coordinate -> Board
set_cell board player coordinate =
  let m = get_map board
      cell = Cell $ Just player
  in Board (Map.insert coordinate cell m)

-- We probably need a destructor for Cell, but I want to refactor all this
-- anyway
cell_to_player :: Cell -> Maybe Player
cell_to_player (Cell x) = x

get_cell_player :: Board -> Coordinate -> Maybe Player
get_cell_player board = cell_to_player . get_cell board

get_cell :: Board -> Coordinate -> Cell
get_cell board coordinate =
  get_map board Map.! coordinate

all_lines :: Board -> Set.Set [Cell]
all_lines board =
   let rows = [map mk_coordinate [(r, A), (r, B), (r, C)] | r <- [A .. C]]
       cols = [map mk_coordinate [(A, c), (B, c), (C, c)] | c <- [A .. C]]
       diag = map (map mk_coordinate)
              [[(A, A), (B, B), (C, C)],
               [(A, C), (B, B), (C, A)]]
   in Set.fromList $ map (map (get_cell board)) $ concat [rows, cols, diag]

winner :: Board -> Maybe Player
winner board =
  let is_winner x = Set.member (replicate 3 (Cell $ Just x)) $ all_lines board
  in List.find is_winner [X, O]

empty_cells :: Board -> Set.Set Coordinate
empty_cells = Map.keysSet . Map.filter (== Cell Nothing) . get_map
