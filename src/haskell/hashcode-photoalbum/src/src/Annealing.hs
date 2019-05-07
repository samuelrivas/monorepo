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

newtype CandidateGen solution = MkGen {
  runCandidateGen :: (Double, solution) -> RVar (Double, solution)
  }

instance Show (CandidateGen s) where
  show _ = "CandidateGen"

data AnnealState solution = AnnealState {
  temp              :: Temp,
  current_iteration :: Integer,
  steps_per_temp    :: Integer,
  cooldown_ratio    :: Double,
  min_cost          :: Double,
  best_solution     :: solution,
  current_cost      :: Double,
  current_solution  :: solution,
  candidate_gen     :: CandidateGen solution
  } deriving Show

initial_state :: (Double, solution) -> AnnealState solution
initial_state (cost, solution) =
  AnnealState {
  temp = 1000,
  current_iteration = 1,
  steps_per_temp = 1000,
  cooldown_ratio = 0.97,
  min_cost = cost,
  best_solution = solution,
  current_cost = cost,
  current_solution = solution,
  candidate_gen = MkGen return
  }

-- | Reduce the temperature if needed
cooldown :: MonadState (AnnealState solution) m => m ()
cooldown =
  do
    steps <- gets steps_per_temp
    iteration <- gets current_iteration
    t <- gets temp
    ratio <- gets cooldown_ratio

    let new_t = t * ratio
    when (iteration `mod` steps == 0) $ modify (\s -> s { temp = new_t })
    modify $ \s -> s { current_iteration = iteration + 1 }

accept_solution :: Double -> StateT (AnnealState state) RVar Bool
accept_solution new_cost =
  do old_cost <- gets current_cost
     t <- gets temp
     let delta = new_cost - old_cost
     let k = 1 / 100
     let e = exp 1 :: Double
     if delta < 0
       then return True
       else
       do coin_flip <- lift . Random.sample $ Random.uniform 0 1
          return $ e ** ((-delta/old_cost)/(k * t)) > coin_flip

anneal_step :: StateT (AnnealState solution) RVar ()
anneal_step = do
  sol <- gets current_solution
  cost <- gets current_cost
  gen  <- gets candidate_gen
  (cost', sol') <- lift . Random.sample $ runCandidateGen gen (cost, sol)
  accept <- accept_solution cost'

  cooldown

  when accept $
    modify $ \s -> s { current_solution = sol', current_cost = cost' }

anneal_to_temp :: Temp -> StateT (AnnealState state) RVar ()
anneal_to_temp cool_temp =
  let hot = (> cool_temp) <$> gets temp
  in whileM_ hot anneal_step

-- Just for testing

f :: Double -> Double
f x = ( x * x + x) * cos (2 * x) + 20

mutate_solution :: Double -> RVar Double
mutate_solution x =
  min 15 . max (-15) <$> Random.normal x 0.5

test_state :: AnnealState Double
test_state =
  let gen (_, s) = do
        candidate <- mutate_solution s
        return (-f candidate, candidate)
  in
    (initial_state (-f 0, 0)) { candidate_gen = MkGen gen }
