module Board
  ( Board
  , Coordinate
  , Player(..)
  , Pos(..)
  , empty_board
  , from_ints
  , get_cell
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

from_ints :: (Pos, Pos) -> Coordinate
from_ints (x, y) = Coordinate(Row x, Column y)

empty_board :: Board
empty_board =
  let positions = [A .. C]
      coordinates = [from_ints (x, y)
                    | x <- positions
                    , y <- positions]
      add = \m c -> Board (Map.insert c (Cell Nothing) (get_map m))
  in foldl add (Board Map.empty) coordinates

show_cell :: Cell -> String
show_cell (Cell (Just x)) = show x
show_cell (Cell Nothing) = " "

show_row :: Board -> Row -> String
show_row board (Row row) =
 let coords = [from_ints (row, col) | col <- [A .. C]]
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
   let rows = [[from_ints (r, A), from_ints (r, B), from_ints (r, C)] | r <- [A .. C]]
       cols = [[from_ints (A, c), from_ints (B, c), from_ints (C, c)] | c <- [A .. C]]
       diag = [[from_ints (A, A), from_ints (B, B), from_ints (C, C)],
               [from_ints (A, C), from_ints (B, B), from_ints (C, A)]]
   in Set.fromList $ List.map (List.map (get_cell board)) $ concat [rows, cols, diag]

winner :: Board -> Maybe Player
winner board =
  let is_winner x = Set.member (replicate 3 (Cell $ Just x)) $ all_lines board
  in List.find is_winner [X, O]
