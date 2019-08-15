{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Game
  (GameState(..),
   StateDistribution(..),
   apply_transition,
   get_transitions,
   force_move,
   force_game,
   is_final,
   force_n_steps
  )
where

import           Prelude                   hiding (head)

import           Control.Monad             (replicateM_)
import           Control.Monad.Loops       (whileJust_)
import           Control.Monad.State.Class (MonadState, get, put)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Random               (Distribution (rvar), MonadRandom,
                                            RVar, sample)
import           Util                      (head)
{-# ANN module "HLint: ignore Use camelCase" #-}

data StateDistribution a =
    Deterministic a
  | Stochastic (RVar a)

instance Distribution StateDistribution a where
  rvar (Deterministic a) = pure a
  rvar (Stochastic a)    = a

-- | The GameState class. We represent the game state as a function taking a
-- transition and returning a distribution of next states
--
-- Note that you need to wrap a specific state for each game, since we need to
-- make the transition type depend on the state in order to prevent ambiguity in
-- the score function
--
-- A state is considered final if the list of transitions from it is empty
class (Ord score, Bounded score)
  => GameState state trans score | state -> trans, state -> score where
  next_state :: (Monad m) => trans -> state -> MaybeT m (StateDistribution state)
  transitions :: state -> [trans]
  score :: state -> score

apply_transition ::
     GameState state transition score
  => MonadState state m
  => MonadRandom m
  => transition -> MaybeT m ()
apply_transition transition =
      get
  >>= next_state transition
  >>= lift . sample
  >>= put

get_transitions ::
     GameState state transition score
  => MonadState state m
  => m [transition]
get_transitions = transitions <$> get

is_final ::
     GameState state transition score
  => MonadState state m
  => m Bool
is_final = not . null <$> get_transitions

force_move ::
     GameState state transition score
  => MonadState state m
  => MonadRandom m
  => MaybeT m ()
force_move = get_transitions >>= head >>= apply_transition

force_game ::
     GameState state transition score
  => MonadState state m
  => MonadRandom m
  => m ()
--force_game = whileJust_ (runMaybeT force_move) return
force_game = whileJust_ (runMaybeT force_move) return

force_n_steps ::
     GameState state transition score
  => MonadState state m
  => MonadRandom m
  => Int -> MaybeT m ()
force_n_steps steps = replicateM_ steps force_move
