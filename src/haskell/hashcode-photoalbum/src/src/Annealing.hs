{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# OPTIONS -Wno-unused-imports -Wno-unused-top-binds #-}
module Annealing
  (
    CandidateGen (..),
    AnnealConfig (..),
    AnnealState (..), --FIXME Don't expose the state!
    anneal_to_temp,
    anneal_to_iteration,
    run_anneal,
    default_config
  ) where

import           Control.Monad         (when)
import           Control.Monad.Loops
import           Control.Monad.Reader  (runReader)
import           Control.Monad.RWS
import           Control.Monad.Writer
import           Data.Functor.Identity (Identity)
import           Data.Random           (RVar)
import qualified Data.Random           as Random
import           Metrics
{-# ANN module "HLint: ignore Use camelCase" #-}

-- Ideas to improve:
--
--   Add metrics
--
--   Make cost a type on its own instead of Double, as it is quite easy to
--   confuse costs and solutions if the problem we were trying to minimise were,
--   for example, a function on real numbers
--
--   Add statistics in the writer about:
--     - The range of delta, as this is useful to tune the temp parameters
--     - Acceptance rate of negatives per temperature
--     - Rate of downhill moves per temperature
--
--   Add anneal_to_steps, and utilities to output metrics and best solution
--   periodically while running in IO

type Temp = Double
type AnnealRWS sol = (AnnealRWST sol) Identity
type AnnealRWST sol = RWST (AnnealConfig sol) Metrics (AnnealState sol)

class MonadRWS (AnnealConfig sol) Metrics (AnnealState sol) m =>
  MonadAnneal sol m

instance MonadAnneal sol (AnnealRWS sol)
instance Monad m => MonadAnneal sol (AnnealRWST sol m)

-- | A Generate a new candidate solution given the current one. It gets also the
-- current cost, and should return the cost of the new solution. It is problem
-- specific wether to prefer to calculate the cost from scratch for every
-- solution or to return a cost delta, so we support both operations by giving
-- enough context
newtype CandidateGen sol = MkGen {
  runCandidateGen :: (Double, sol) -> RVar (Double, sol)
  }

instance Show (CandidateGen s) where
  show _ = "CandidateGen"

data AnnealConfig solution = AnnealConfig {
  initial_temp   :: Temp,
  steps_per_temp :: Integer,
  cooldown_ratio :: Double,
  candidate_gen  :: CandidateGen solution
  } deriving Show

data AnnealState sol = AnnealState {
  temp              :: Temp,
  current_iteration :: Integer,
  min_cost          :: Double,
  best_sol          :: sol,
  current_cost      :: Double,
  current_solution  :: sol
  } deriving Show

initial_state :: MonadReader (AnnealConfig sol) m =>
  (Double, sol) -> m (AnnealState sol)
initial_state (cost, sol) =
  do
    t <- asks initial_temp
    return $ AnnealState {
      temp = t,
      current_iteration = 1,
      min_cost = cost,
      best_sol = sol,
      current_cost = cost,
      current_solution = sol
      }

default_config :: CandidateGen sol -> AnnealConfig sol
default_config gen =
  AnnealConfig {
  initial_temp = 1000,
  steps_per_temp = 1000,
  cooldown_ratio = 0.97,
  candidate_gen = gen
  }

-- | Reduce the temperature if needed
-- cooldown :: MonadRWS r w (AnnealState solution) m => m ()
cooldown :: MonadAnneal solution m => m ()
cooldown =
  do
    steps <- asks steps_per_temp
    iteration <- gets current_iteration
    t <- gets temp
    ratio <- asks cooldown_ratio

    let new_t = t * ratio
    when (iteration `mod` steps == 0) $ do
      modify (\s -> s { temp = new_t })
      increment_counter "annealing.cooldown"

    modify $ \s -> s { current_iteration = iteration + 1 }

accept_solution :: Double -> (AnnealRWST solution) RVar Bool
accept_solution new_cost =
  do old_cost <- gets current_cost
     t <- gets temp
     let delta = new_cost - old_cost
     let p = exp ((-delta)/t)
     if delta < 0
       then
       do
         increment_counter "annealing.energy_lowered"
         return True
       else
       do coin_flip <- lift $ Random.uniform 0 1
          increment_counter "annealing.energy_increased"
          let accept = p > coin_flip
          when accept $ increment_counter "annealing.random_accept"
          return accept

anneal_step :: (AnnealRWST solution) RVar ()
anneal_step = do
  sol <- gets current_solution
  cost <- gets current_cost
  best_cost <- gets min_cost
  gen  <- asks candidate_gen
  (cost', sol') <- lift $ runCandidateGen gen (cost, sol)
  accept <- accept_solution cost'

  cooldown

  increment_counter "annealing.solution_proposed"
  when (cost' < best_cost) $ do
    modify $ \s -> s { best_sol = sol', min_cost = cost' }
    increment_counter "annealing.solution_improved"

  when accept $ do
    increment_counter "annealing.solution_accepted"
    modify $ \s -> s { current_solution = sol', current_cost = cost' }

anneal_to_temp :: Temp -> (AnnealRWST solution) RVar ()
anneal_to_temp cool_temp =
  let hot = (> cool_temp) <$> gets temp
  in whileM_ hot anneal_step

anneal_to_iteration :: Integer -> (AnnealRWST solution) RVar ()
anneal_to_iteration iteration =
  let not_passed = (<= iteration) <$> gets current_iteration
  in whileM_ not_passed anneal_step

run_anneal :: (AnnealRWST solution) RVar ()
  -> AnnealConfig solution
  -> (Double, solution)
  -> WriterT Metrics RVar (Double, solution)
run_anneal computation config starting_point =
  let s = runReader (initial_state starting_point) config
  in WriterT $ do
    ((), end_state, w) <- runRWST computation config s
    return ((min_cost end_state, best_sol end_state), w)
