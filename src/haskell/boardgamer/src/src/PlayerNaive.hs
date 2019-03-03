module PlayerNaive (
  mk_player
  ) where

import qualified GameState
import qualified Player

{-# ANN module "HLint: ignore Use camelCase" #-}

mk_player :: Player.Player
mk_player = Player.mk_player next_move

next_move :: GameState.State -> IO GameState.Move
next_move state = return . head $ GameState.possible_moves state
