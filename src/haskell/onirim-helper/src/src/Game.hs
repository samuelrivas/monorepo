{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Game
  (GameState(..),
   StateDistribution(..),
   apply_transition,
   force_move,
   force_game,
   is_final,
   force_n_steps
  )
where

import           Prelude                    hiding (head)

import           Control.Monad              (replicateM_)
import           Control.Monad.Fail         (MonadFail)
import           Control.Monad.Loops        (whileJust_)
import           Control.Monad.Random       (RandomGen)
import           Control.Monad.Reader       (asks, runReaderT)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.State.Class  (MonadState, get, put)
import           Control.Monad.Trans.Maybe  (MaybeT (..), runMaybeT)
import           Data.Random                (Distribution (rvar), RVar, sample)
import           Data.Random.Sample         (samplePure)
import           Util                       (head)

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
  next_state :: (MonadReader state m, MonadFail m) =>
    trans -> m (StateDistribution state)
  transitions :: MonadReader state m => m [trans]
  score :: state -> score

apply_transition ::
     RandomGen g
  => GameState state transition score
  => MonadState state m
  => MonadFail m
  => g -> transition -> m g
apply_transition g transition =
  do
    d <- get >>= runReaderT (next_state transition)
    let (newState, newG) = samplePure d g
    put newState
    return newG

is_final ::
     GameState state transition score
  => MonadReader state m
  => m Bool
is_final = asks (not . null . transitions)

-- Note that @head . transitions@ works by using the basic reader instance,
-- which happens to be just a function from state to a list of transitions
force_move ::
     RandomGen g
  => GameState state transition score
  => MonadState state m
  => MonadFail m
  => g -> m g
force_move g = get >>= head . transitions >>= apply_transition g

force_game ::
     RandomGen g
  => GameState state transition score
  => MonadState state m
  => g -> m g
force_game g =
  runMaybeT (force_move g) >>= \case
  Just newG -> force_game newG
  Nothing   -> return g

force_n_steps ::
     RandomGen g
  => GameState state transition score
  => MonadState state m
  => MonadFail m
  => g -> Int -> m g
force_n_steps g steps
  | steps < 0 = return g
  | otherwise = force_move g >>= flip force_n_steps (steps - 1)

