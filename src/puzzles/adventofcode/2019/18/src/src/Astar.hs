-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Astar where

import           Prelude

import           Control.Lens              (assign, at, modifying, uses, view)
import           Control.Monad.Fail        (MonadFail)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (Reader, runReader)
import           Control.Monad.RWS.CPS     (MonadReader, MonadState,
                                            MonadWriter, RWST, runRWST)
import           Control.Monad.Trans.Class (MonadTrans)
import           Data.Generics.Labels      ()
import           Data.Hashable             (Hashable)
import qualified Data.HashSet              as HashSet
import qualified Data.Map.Strict           as Map

import           AstarInternal
import           MonadSearch

newtype AstarT node pc w m a = AstarT {
  unAstarT :: RWST (AstarConfig node pc) w (AstarContext node) m a
  } deriving newtype (Functor, Applicative, Monad, MonadWriter w,
                      MonadState (AstarContext node),
                      MonadReader (AstarConfig node pc), MonadIO, MonadFail,
                      MonadTrans)

instance (Eq node, Hashable node , Monad m, Monoid w)
  => MonadSearch node (AstarT node pc w m) where
  popNode = popBest
  pushNode = pushAstarNode
  seenNode = seenAstarNode
  goalNode = astarGetGoalNode
  explode  = astarExplodeNode

mkConfig ::
  (node -> Reader pc Int) -> -- h
  (node -> Reader pc Int) -> -- c
  (node -> Reader pc [node]) -> -- explode
  (node -> Reader pc Bool) -> -- isGoal
  pc ->
  AstarConfig node pc
mkConfig = AstarConfig

runAstarT ::
  Monoid w
  => AstarT node pc w m a
  -> AstarConfig node pc
  -> AstarContext node
  -> m (a, AstarContext node, w)
runAstarT = runRWST . unAstarT

evalAstarT ::
  Monoid w => Eq node => Hashable node => Monad m =>
  AstarConfig node pc -> node -> m (Maybe node, w)
evalAstarT astarConfig initialNode  =
  let
    initialContext = AstarContext Map.empty HashSet.empty
  in do
    (nodeM, _, w) <- runAstarT
                       (pushAstarNode initialNode >> search)
                       astarConfig
                       initialContext
    pure (nodeM, w)

popBest :: Monad m => AstarT node pc w m (Maybe node)
popBest =
  uses #openNodes Map.minView >>= \case
    Just (node, newMap) -> do
      assign #openNodes newMap
      pure $ Just node
    Nothing -> pure Nothing

valueNode :: Monad m => node -> AstarT node pc w m Int
valueNode node = do
  hReader <- view #h <*> pure node
  cReader <- view #c <*> pure node
  runInPrivateContext $ (+) <$> hReader <*> cReader

astarGetGoalNode :: Monad m => node -> AstarT node pc w m Bool
astarGetGoalNode node = view #isGoal <*> pure node >>= runInPrivateContext

astarExplodeNode :: Monad m => node -> AstarT node pc w m [node]
astarExplodeNode node = view #explode <*> pure node >>= runInPrivateContext

runInPrivateContext :: Monad m => Reader pc a -> AstarT node pc w m a
runInPrivateContext reader = runReader reader <$> view #privateContext

seenAstarNode ::
  Eq node => Hashable node => Monad m =>
  node -> AstarT node pc w m Bool
seenAstarNode node = uses #seenNodes (HashSet.member node)

pushAstarNode ::
  Eq node => Hashable node => Monad m =>
  node -> AstarT node pc w m ()
pushAstarNode node = do
  value <- valueNode node
  assign (#openNodes . at value) . Just $ node
  modifying #seenNodes (HashSet.insert node)
