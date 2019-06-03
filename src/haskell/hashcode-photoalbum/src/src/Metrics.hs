{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Very basic metrics collection as a writer monad

module Metrics
  ( Metrics (..)
  , merge
  , add_to_histogram
  -- , increment_counter
  -- , increment_counter_n
  ) where

import           Control.Monad.Writer.Strict
import           Data.IntMultiSet            (IntMultiSet)
import qualified Data.IntMultiSet            as IntMultiSet
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Numeric.Natural             (Natural)

{-# ANN module "HLint: ignore Use camelCase" #-}

type Key = String
-- data Metrics = Metrics {
--   counters   :: Map Key (Sum Natural),
--   histograms :: Map Key IntMultiSet
--   } deriving Show
data Metrics = Metrics
  deriving Show

instance Semigroup Metrics where
  (<>) = merge
instance Monoid Metrics where
  -- mempty = Metrics Map.empty Map.empty
  mempty = Metrics

merge :: Metrics -> Metrics -> Metrics
merge !x !y = mempty
  -- Metrics {
  -- counters = Map.unionWith (<>) (counters x) (counters y),
  -- histograms = Map.unionWith (<>) (histograms x) (histograms y)
  -- }

count_step :: String -> Metrics
count_step name = count_steps name 1

count_steps :: String -> Natural -> Metrics
-- count_steps name steps = mempty {
--   counters = Map.singleton name (Sum steps)
--   }
count_steps name steps = Metrics

histogram_point :: String -> Int -> Metrics
-- histogram_point name value = mempty {
--   histograms = Map.singleton name (IntMultiSet.singleton value)
--   }
histogram_point _ _ = Metrics

add_to_histogram :: (MonadWriter Metrics m) => String -> Int -> m ()
add_to_histogram name  = tell . histogram_point name

increment_counter :: MonadWriter Metrics m => String -> m ()
increment_counter _ = pure ()
-- increment_counter = tell . count_step

increment_counter_n :: (MonadWriter Metrics m) => String -> Natural -> m ()
increment_counter_n name = tell . count_steps name . fromIntegral
