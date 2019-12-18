{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Astar where

import           Prelude

import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import Control.Lens (assign, set, at, use, view, modifying, uses)
import Control.Monad.RWS.CPS (lift, RWST, MonadState, MonadReader, tell, MonadWriter, runRWST, asks)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Applicative (Alternative (..))
import Data.Generics.Labels ()
import Data.Hashable (Hashable)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)

import AstarInternal
import MonadSearch

type Heuristic node = node -> Int
type CostFunction node = node -> Int
type Explode node = node -> [node]

newtype AstarT node w m a = AstarT {
  unAstarT :: RWST (AstarConfig node) w (AstarContext node) m a
  } deriving newtype (Functor, Applicative, Monad, MonadWriter w,
                      MonadState (AstarContext node),
                      MonadReader (AstarConfig node), MonadIO, MonadFail,
                      MonadTrans)

instance (Eq node, Hashable node , Monad m, Monoid w)
  => MonadSearch node (AstarT node w m) where
  popNode = popBest
  pushNode = pushAstarNode
  seenNode = seenAstarNode
  goalNode node = asks (view #isGoal) <*> pure node
  explode node = asks (view #explode) <*> pure node

runAstarT ::
  Monoid w
  => AstarT node w m a
  -> AstarConfig node
  -> AstarContext node
  -> m (a, AstarContext node, w)
runAstarT = runRWST . unAstarT

popBest :: Monad m => AstarT node w m (Maybe node)
popBest =
  uses #openNodes Map.minView >>= \case
    Just (node, newMap) -> do
      assign #openNodes newMap
      pure $ Just node
    Nothing -> pure Nothing

valueNode :: Monad m => node -> AstarT node w m Int
valueNode node = do
  h <- asks $ view #h
  c <- asks $ view #c
  pure $ h node + c node

seenAstarNode ::
  Eq node => Hashable node => Monad m
  => node -> AstarT node w m Bool
seenAstarNode node = uses #seenNodes (HashSet.member node)

pushAstarNode ::
  Eq node =>
  Hashable node =>
  Monad m => node -> AstarT node w m ()
pushAstarNode node = do
  value <- valueNode node
  assign (#openNodes . at value) . Just $ node
  modifying #seenNodes (HashSet.insert node)
