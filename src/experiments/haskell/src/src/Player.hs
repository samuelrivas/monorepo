module Player
  ( Player
  , mk_player
  , next_move
  ) where

import qualified GameState

{-# ANN module "HLint: ignore Use camelCase" #-}

data Player = Player {
  next_move_f :: GameState.State -> IO GameState.Move
  }

mk_player :: (GameState.State -> IO GameState.Move) -> Player.Player
mk_player f = Player { next_move_f = f }

next_move :: GameState.State -> Player -> IO GameState.Move
next_move state player = next_move_f player state
