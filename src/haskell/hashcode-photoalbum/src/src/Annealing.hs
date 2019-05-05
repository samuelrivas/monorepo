{-# LANGUAGE FlexibleContexts #-}
-- {-# OPTIONS -Wno-unused-imports -Wno-unused-top-binds #-}
module Annealing
  (
    anneal_to_temp,
    initial_state
  ) where

import           Control.Monad.Loops
import           Control.Monad.State
import           Data.Random         (RVar, RVarT)
import qualified Data.Random         as Random
{-# ANN module "HLint: ignore Use camelCase" #-}

type Temp = Double

instance Show (RVarT m a) where
  show _ = "RVarT"

data AnnealState cost solution = AnnealState {
  temp              :: Temp,
  current_iteration :: Integer,
  steps_per_temp    :: Integer,
  cooldown_ratio    :: Double,
  min_cost          :: cost,
  best_solution     :: solution,
  current_cost      :: cost,
  current_solution  :: solution,
  candidate_gen     :: RVar (cost, solution)
  } deriving Show

initial_state :: (cost, solution) -> AnnealState cost solution
initial_state (cost, solution) =
  AnnealState {
  temp = 100,
  current_iteration = 1,
  steps_per_temp = 10,
  cooldown_ratio = 0.99,
  min_cost = cost,
  best_solution = solution,
  current_cost = cost,
  current_solution = solution,
  candidate_gen = return (cost, solution)
  }

-- | Reduce the temperature if needed
cooldown :: MonadState (AnnealState cost solution) m => m ()
cooldown =
  do
    steps <- gets steps_per_temp
    iteration <- gets current_iteration
    t <- gets temp
    ratio <- gets cooldown_ratio

    let new_t = t * ratio
    when (iteration `mod` steps == 0) $ modify (\s -> s { temp = new_t })
    modify $ \s -> s { current_iteration = iteration + 1 }

accept_solution :: Ord cost => cost -> StateT (AnnealState cost state) RVar Bool
accept_solution new_cost = (new_cost <) <$> gets current_cost

anneal_step :: Ord cost => StateT (AnnealState cost state) RVar (cost, state)
anneal_step = do
  sol <- gets current_solution
  cost <- gets current_cost
  (cost', sol') <- gets candidate_gen >>= lift . Random.sample
  accept <- accept_solution cost'

  cooldown

  return $ if accept
           then (cost', sol')
           else (cost, sol)


anneal_to_temp :: Ord cost =>Temp -> StateT (AnnealState cost state) RVar ()
anneal_to_temp cool_temp =
  let hot = (> cool_temp) <$> gets temp
  in whileM_ hot anneal_step
