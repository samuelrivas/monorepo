module GameState (
  State,
  Move,
  initial_state,
  possible_moves,
  mk_move,
  next_state,
  players,
  scores,
  ) where

import qualified Board         as Board
import qualified Data.Map.Lazy as Map
import qualified Data.Maybe    as Maybe
import qualified Data.Set      as Set

data Move = Move { get_player     :: Board.Player
                 , get_coordinate :: Board.Coordinate
                 }
            deriving (Show, Eq)

data State = State { get_board          :: Board.Board
                   , get_control_player :: Board.Player
                   }

instance Show State where
  show state = (show $ get_board state)
               ++ "Playing "
               ++ (show $ get_control_player state)

initial_state :: State
initial_state = State { get_board = Board.empty_board
                      , get_control_player = Board.X
                      }

-- The list of possible moves can be infinite
possible_moves :: State -> [Move]
possible_moves state =
  let
    player = get_control_player state
    board = get_board state
    moves = [ Move { get_player = player
                   , get_coordinate = coordinate
                   }
            | coordinate <- Set.toList $ Board.empty_cells board
            ]
  in if Maybe.isJust $ Board.winner board
     then []
     else moves

mk_move :: State -> Board.Coordinate -> Maybe Move
mk_move state coordinate =
  let move = Move { get_player = get_control_player state
                  , get_coordinate = coordinate
                  }
  in if legal_move state move
     then Just move
     else Nothing

legal_move :: State -> Move -> Bool
legal_move state move =
  let
    board = get_board state
    coordinate = get_coordinate move
    control_player = get_control_player state
  in
    (control_player == get_player move)
    && (Maybe.isNothing $ Board.get_cell_player board coordinate)

next_state :: State -> Move -> Maybe State
next_state state move =
  let
    board = get_board state
    player = get_player move
    coordinate = get_coordinate move
  in
  if legal_move state move then
    Just $ State { get_board = Board.set_cell board player coordinate
                 , get_control_player = swap_player player
                 }
  else
    Nothing

players :: Set.Set Board.Player
players = Set.fromList [Board.X, Board.O]

swap_player :: Board.Player -> Board.Player
swap_player Board.X = Board.O
swap_player Board.O = Board.X

-- The bool value is whether the player won or not
score :: Num a => Bool -> a
score True = 100
score False = 0

player_score :: Num a => State -> Board.Player -> a
player_score state player =
  let
    board = get_board state
  in
    Maybe.fromMaybe 0 (score . (== player) <$> Board.winner board)

scores :: Num a => State -> Map.Map Board.Player a
scores state =
  let player_list = Set.toList players
  in
  Map.fromList $ zip player_list (player_score state <$> player_list)
