-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module AstarInternal (
  Heuristic,
  CostFunction,
  AstarContext (AstarContext),
  AstarConfig (AstarConfig)
  ) where

import           Data.Map.Strict (Map)
import Data.HashSet (HashSet)
import           GHC.Generics    (Generic)

type Heuristic node = node -> Int
type CostFunction node = node -> Int

data AstarContext node = AstarContext {
  openNodes :: Map Int node,
  seenNodes :: HashSet node
  } deriving stock (Show, Generic)

data AstarConfig node = AstarConfig {
  h       :: Heuristic node,
  c       :: CostFunction node,
  explode :: node -> [node],
  isGoal  :: node -> Bool
  } deriving stock (Generic)

instance Show (AstarConfig node) where
  show = const "AstarConfig"
