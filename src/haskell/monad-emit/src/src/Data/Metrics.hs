{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Data.Metrics (
  Metrics (..),
  Count (Count),
  Gauge,
  Max (Max),
  Min (Min),
  gaugeEntry
  ) where
import           Perlude

import           Data.Metrics.Internal
import           Data.Monoid           (Sum (..))

-- |Generate a gauge entry representing one data point
gaugeEntry :: Num n => Integral i => n -> Gauge i n
gaugeEntry n = Gauge (Count 1) (Sum n) (Max n) (Min n)
