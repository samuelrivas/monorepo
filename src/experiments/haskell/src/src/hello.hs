-- TODO: Consider make the board a state monad

import qualified Data.Map.Lazy as Map
import Data.List

data Pos = A | B | C
         deriving (Eq, Show, Bounded, Enum, Ord)

newtype Column = Column Pos deriving (Eq, Ord)
newtype Row = Row Pos deriving (Eq, Ord)

instance Show Column where
  show (Column x) = "Column(" ++ (show x) ++ ")"

instance Show Row where
  show (Row x) = "Row(" ++ (show x) ++ ")"

newtype Coordinate = Coordinate (Row, Column) deriving (Eq, Ord, Show)

data Player = X | O deriving Show
newtype Cell = Cell (Maybe Player)

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
 in "| " ++ (intercalate " | " $ map show cells) ++ " |"

show_board :: Board -> String
show_board board =
  let rows = [Row pos | pos <- [A .. C]]
      shown_rows = map (show_row board) rows
      sep = "+---+---+---+"
  in unlines $ [sep] ++ (intersperse sep shown_rows) ++ [sep]

set_cell :: Board -> Player -> Coordinate -> Board
set_cell board player coordinate =
  let m = get_map board
      cell = Cell $ Just player
  in Board (Map.insert coordinate cell m)

main :: IO ()
main = putStrLn $ show empty_board
