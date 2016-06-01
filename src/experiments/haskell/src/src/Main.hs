-- TODO: Consider make the board a state monad

import qualified Board
import qualified Data.Map.Lazy as Map
import qualified GameState
import qualified Player
import qualified PlayerHuman   as Human

{-# ANN module "HLint: ignore Use camelCase" #-}

turn :: GameState.State -> IO GameState.State
turn state =
  let player = select_player $ GameState.get_control_player state
  in do
    move <- Player.next_move state player
    case GameState.next_state state move of
      Just next_state -> return next_state
      Nothing -> force_move state

select_player :: Board.Player -> Player.Player
select_player _ = Human.mk_player

force_move :: GameState.State -> IO GameState.State
force_move state =
  let player = GameState.get_control_player state
  in do
  print $ "Player " ++ show player ++ " didn't return a valid move, forcing one"
  return $ fst $ GameState.force_next_state state

game_loop :: GameState.State -> IO GameState.State
game_loop state =
  let possible_moves = GameState.possible_moves state
      more_moves = not (null possible_moves)
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
  print "The game has ended"
  print end_state
  show_winner end_state
