-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Internal (
  NetworkState (NetworkState),
  NodeState (NodeState)
  ) where

import           Data.HashMap.Strict (HashMap)
import           Data.Monoid         (Sum)
import           GHC.Generics        (Generic)

import           Intcode

data NodeState = NodeState {
  computerState :: IntcodeState,
  input         :: Maybe [Integer]
  } deriving stock (Show, Generic)

data NetworkState = NetworkState {
  nodes     :: HashMap Integer NodeState,
  nat       :: Maybe (Integer, Integer),
  lastNat   :: Maybe (Integer, Integer),
  idle      :: Bool,
  dupNats   :: [Integer],
  idleCount :: Sum Integer
  } deriving stock (Show, Generic)
