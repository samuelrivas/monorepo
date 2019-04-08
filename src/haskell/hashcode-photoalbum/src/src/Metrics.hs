{-# LANGUAGE FlexibleContexts #-}
-- | Very basic metrics collection as a writer monad

module Metrics
  ( Metrics (..)
  , merge
  , increment_histogram
  , increment_counter
  ) where

import           Control.Monad.Writer
import           Data.IntMultiSet     (IntMultiSet)
import qualified Data.IntMultiSet     as IntMultiSet
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Numeric.Natural      (Natural)

{-# ANN module "HLint: ignore Use camelCase" #-}

type Key = String
data Metrics = Metrics {
  counters   :: Map Key (Sum Natural),
  histograms :: Map Key IntMultiSet
  } deriving Show

instance Semigroup Metrics where
  (<>) = merge
instance Monoid Metrics where
  mempty = Metrics Map.empty Map.empty

merge :: Metrics -> Metrics -> Metrics
merge x y =
  Metrics {
  counters = Map.unionWith (<>) (counters x) (counters y),
  histograms = Map.unionWith (<>) (histograms x) (histograms y)
  }

count_step :: String -> Metrics
count_step name = mempty {
  counters = Map.singleton name 1
  }

histogram_point :: String -> Int -> Metrics
histogram_point name value = mempty {
  histograms = Map.singleton name (IntMultiSet.singleton value)
  }

increment_histogram :: (MonadWriter Metrics m) => String -> Int -> m ()
increment_histogram name  = tell . histogram_point name

increment_counter :: MonadWriter Metrics m => String -> m ()
increment_counter = tell . count_step
