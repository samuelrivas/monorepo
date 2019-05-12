-- Quick hack to test simmulated annealing
module TestAnnealing where
import           Annealing
import           Data.Random (RVar, normal)
{-# ANN module "HLint: ignore Use camelCase" #-}

f :: Double -> Double
f x = ( x * x + x) * cos (2 * x) + 20

mutate_solution :: Double -> RVar Double
mutate_solution x = min 15 . max (-15) <$> normal x 0.5

test_gen :: CandidateGen Double
test_gen = MkGen $
  \(_, s) ->
    do
      candidate <- mutate_solution s
      return (-f candidate, candidate)
