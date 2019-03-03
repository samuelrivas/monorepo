module PlayerMinMax (
  mk_player
  ) where

import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord   as Ord
import qualified Data.Set   as Set
import qualified GameState
import qualified Player

{-# ANN module "HLint: ignore Use camelCase" #-}

data GameTree move state =
  GameTree { get_state    :: state
           , get_children :: [(move, GameTree move state)]
           }

mk_player :: Player.Player
mk_player = Player.mk_player next_move

next_move :: GameState.State -> IO GameState.Move
next_move = return . select_move . heuristic_game_tree

-- We just add our points and remove the opponent points
-- I am not sure this will work for more than two player games, but is ok for
-- now
h :: GameState.Player -> GameState.State -> Int
h we state =
  let accumulate acc player =
        let op = if player == we then (+) else (-)
        in op acc (GameState.player_score state player)
  in
    Set.foldl accumulate 0 GameState.players

-- TODO: use proper lenses to map over the second element of the exploded tuples
mk_game_tree :: (state -> [(move, state)]) -> state -> GameTree move state
mk_game_tree explode root =
  let (moves, next_states) = unzip $ explode root
  in
  GameTree { get_state = root
           , get_children = zip moves $ map (mk_game_tree explode) next_states
           }

game_tree :: GameState.State -> GameTree GameState.Move GameState.State
game_tree state =
  let
    next_state st move = do
      s <- GameState.next_state st move
      return (move, s)
    explode st = Maybe.mapMaybe (next_state st) (GameState.possible_moves st)
  in
  mk_game_tree explode state

tree_map :: (state -> b) -> GameTree move state -> GameTree move b
tree_map f tree =
  let (moves, next_states) = unzip $ get_children tree
  in
  GameTree { get_state = f $ get_state tree
           , get_children = zip moves $ map (tree_map f) next_states
           }

heuristic_game_tree :: GameState.State ->
                       GameTree GameState.Move Int
heuristic_game_tree state =
  let we = GameState.get_control_player state
  in
    tree_map (h we) (game_tree state)

select_move :: GameTree GameState.Move Int -> GameState.Move
select_move htree =
  let
    children = get_children htree
    minmax (m, ht) = (m, minimise ht)
  in
    fst . List.maximumBy (Ord.comparing snd) $ map minmax children

maximise :: GameTree m Int -> Int
maximise tree =
  let nodes = map snd $ get_children tree
  in
    if null nodes then
      get_state tree
    else
      maximum $ map minimise nodes

minimise :: GameTree m Int -> Int
minimise tree =
  let nodes = map snd $ get_children tree
  in
    if null nodes then
      get_state tree
    else
      minimum $ map maximise nodes
