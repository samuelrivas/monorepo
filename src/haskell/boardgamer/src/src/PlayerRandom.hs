module PlayerRandom (
  mk_player
  ) where

import qualified Data.Random as Random
import qualified GameState
import qualified Player

{-# ANN module "HLint: ignore Use camelCase" #-}

mk_player :: Player.Player
mk_player = Player.mk_player next_move

next_move :: GameState.State -> IO GameState.Move
next_move state =
  let random_move = Random.randomElement $ GameState.possible_moves state
  in Random.runRVar random_move Random.StdRandom
