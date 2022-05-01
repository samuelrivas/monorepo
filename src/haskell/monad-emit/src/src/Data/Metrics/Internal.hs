{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Metrics.Internal (
  Count (Count),
  Gauge (Gauge),
  Metrics (Metrics),
  Max (Max),
  Min (Min),
  ) where

import           Perlude

import qualified Prelude

import           Control.Lens         (coerced, over, view, views)
import           Data.Generics.Labels ()
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HashMap
import           Data.Monoid          (Sum (..))
import qualified Data.Text            as Text
import           GHC.Generics         (Generic)
import           Text.Printf          (PrintfArg, printf)

-- Min, Max and Count wrappers

newtype Min n = Min { getMin :: n } deriving stock (Eq, Generic, Show)
newtype Max n = Max { getMax :: n } deriving stock (Eq, Generic, Show)
newtype Count n = Count { getCount :: n }
  deriving stock (Eq, Generic)
  deriving (Semigroup, Monoid) via (Sum n)

instance Ord a => Semigroup (Min a) where
  Min a <> Min b = Min $ Perlude.min a b

instance Ord a => Semigroup (Max a) where
  Max a <> Max b = Max $ Perlude.max a b

instance (Ord a, Bounded a) => Monoid (Min a) where
  mempty = Min maxBound

instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty = Max minBound

instance Show n => Show (Count n) where
  show c = Text.unpack $ "count " <> views #getCount show c

-- Gauge, a datatype to accumulate gauge metrics and keep track of basic stats

data Gauge int num = Gauge {
  entries :: Count int,
  total   :: Sum num,
  max     :: Max num,
  min     :: Min num
  } deriving stock (Eq, Generic)

instance (Integral i, Num n, Ord n) => Semigroup (Gauge i n) where
  a <> b = Gauge
    (view #entries a <> view #entries b)
    (view #total a <> view #total b)
    (view #max a <> view #max b)
    (view #min a <> view #min b)

instance (Integral i, Num n, Bounded n, Ord n) => Monoid (Gauge i n) where
  mempty = Gauge mempty mempty mempty mempty
  mappend = (<>)

instance (Integral i, Real n, PrintfArg n, Show n, Show i)
  => Show (Gauge i n) where
  show = Text.unpack . showGauge

-- Metrics, a named collection of gauges and counters that a MonadEmit can emit

data Metrics int num = Metrics {
  gauges :: HashMap Text (Gauge int num),
  counts :: HashMap Text (Count int)
  }
  deriving stock (Eq, Generic)

instance (Num n, Ord n, Integral i) => Semigroup (Metrics i n) where
  (<>) a = over #gauges (HashMap.unionWith (<>) (view #gauges a))
           . over #counts (HashMap.unionWith (<>) (view #counts a))

instance (Integral i, Num n, Ord n) => Monoid (Metrics i n) where
  mappend = (<>)
  mempty = Metrics HashMap.empty HashMap.empty

instance (Integral i, Real r, PrintfArg r, Show i, Show r)
  => Show (Metrics i r) where
  show ms =
    Text.unpack . Text.intercalate "\n" $ [
    views #gauges printMap ms,
    views #counts printMap ms]

printMap :: Show a => HashMap Text a -> Text
printMap =
  Text.intercalate "\n"
  . HashMap.foldlWithKey' (\acc desc v -> (desc <> ": " <> show v) : acc) []

average :: forall n i f.Integral i => Real n => RealFrac f =>  Gauge i n -> f
average g =
  let
    es :: i = view (#entries . coerced) g
    vs :: n = view (#total . coerced) g
  in
    realToFrac vs / fromIntegral es

showGauge ::
  Integral i => Real n => PrintfArg n => Show n => Show i
  => Gauge i n -> Text
showGauge g =
  let
    avg :: Double = average g
  in
    "(" <> views (#min . #getMin) show g <> " | "
    <> Text.pack (printf "%.4f" avg)
    <> " | " <> views (#max . #getMax) show g <> ") "
    <> views #total (show . getSum) g <> "/"
    <> views (#entries . #getCount) show g
