module PlayerMinMax (
  mk_player
  ) where

import qualified Data.Map.Lazy as Map
import qualified Data.Set      as Set
import qualified GameState
import qualified Player

{-# ANN module "HLint: ignore Use camelCase" #-}

mk_player :: Player.Player
mk_player = Player.mk_player next_move

next_move :: GameState.State -> IO GameState.Move
next_move state = undefined

-- We just add our points and remove the opponent points
-- I am not sure this will work for more than two player games, but is ok for
-- now
h :: Num a => GameState.State -> a
h state =
  let we = GameState.get_control_player state
      accumulate acc player =
        let op = if player == we then (+) else (-)
        in op acc (GameState.player_score state player)
  in
    Set.foldl accumulate 0 GameState.players
