{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Monad.MonadSearch.Astar (
  AstarConfig,
  AstarT,
  mkConfig,
  runAstarT,
  runInAstarT,
  searchAstarT,
  peekBest
  ) where

import           Perlude

import           Control.Lens                            (assign, modifying,
                                                          use, uses, view)
import           Control.Monad.MonadEmit                 (Metrics, MonadEmit,
                                                          emit, emitCount,
                                                          emitGauge)
import           Control.Monad.MonadSearch               (MonadSearch (..),
                                                          search)
import           Control.Monad.RWS.CPS                   (MonadReader,
                                                          MonadState,
                                                          MonadWriter, RWST,
                                                          runRWST)
import           Control.Monad.Reader                    (Reader, runReader)
import           Control.Monad.Trans.Class               (MonadTrans, lift)
import           Data.Generics.Labels                    ()
import           Data.Hashable                           (Hashable)
import qualified Data.PriorityQueue.FingerTree           as PQueue

import           Control.Monad.MonadSearch.AstarInternal

newtype AstarT n node nodeStore pc w m a = AstarT {
  unAstarT :: RWST (AstarConfig n node nodeStore pc) w (AstarContext n node nodeStore) m a
  } deriving newtype (Functor, Applicative, Monad, MonadWriter w,
                      MonadState (AstarContext n node nodeStore),
                      MonadReader (AstarConfig n node nodeStore pc), MonadIO,
                      MonadFail, MonadTrans)

instance (MonadEmit metrics m) =>
  MonadEmit metrics (AstarT n node nodeStore pc w m) where
  emit = lift . emit

instance
  (Show node,
   Eq node,
   Hashable node,
   Integral i,
   Num n,
   Ord n,
   MonadEmit (Metrics i n) m) =>
  MonadSearch node (AstarT n node nodeStore pc w m) where
  popNode = popBest
  pushNode = astarPushNode
  seenNode = astarSeenNode
  goalNode = astarGetGoalNode
  explode  = astarExplodeNode
  markSeen = astarMarkSeen

mkConfig ::
  (node -> Reader pc n) -> -- h
  (node -> Reader pc n) -> -- c
  (node -> Reader pc [node]) -> -- explode
  (node -> Reader pc Bool) -> -- isGoal
  (nodeStore -> node -> Reader pc nodeStore) -> -- rememberNode
  (nodeStore -> node -> Reader pc Bool) -> -- rememberNode
  pc ->
  AstarConfig n node nodeStore pc
mkConfig = AstarConfig

runAstarT ::
  Monoid w
  => AstarT n node nodeStore pc w m a
  -> AstarConfig n node nodeStore pc
  -> AstarContext n node nodeStore
  -> m (a, AstarContext n node nodeStore, w)
runAstarT = runRWST . unAstarT

searchAstarT ::
  Num n =>
  Integral i =>
  Ord n =>
  Monoid w =>
  Eq node =>
  Show node =>
  Hashable node =>
  MonadEmit (Metrics i n) m =>
  AstarConfig n node nodeStore pc -> node -> nodeStore -> m (Maybe node, w)
searchAstarT astarConfig initialNode initialStore  = do
  (nodeM, _, w) <- runInAstarT search astarConfig initialNode initialStore
  pure (nodeM, w)

runInAstarT ::
  Integral i => Num n => Ord n => Monoid w => Show node => Eq node
  => Hashable node
  => MonadEmit (Metrics i n) m
  => AstarT n node nodeStore pc w m a
  -> AstarConfig n node nodeStore pc -> node -> nodeStore
  -> m (a, AstarContext n node nodeStore, w)
runInAstarT x astarConfig initialNode initialStore =
  let initialContext = AstarContext PQueue.empty initialStore
  in runAstarT
       (astarPushNode initialNode >> x)
       astarConfig
       initialContext

popBest ::
  MonadEmit (Metrics i n) m
  => Integral i
  => Ord n
  => Num n
  => Show node
  => Eq node
  => Hashable node
  => AstarT n node nodeStore pc w m (Maybe node)
popBest =
  uses #openNodes PQueue.minView >>= \case
    Just (node, newMap) -> do
      hReader <- view #h <*> pure node
      cReader <- view #c <*> pure node
      h <- runInPrivateContext hReader
      c <- runInPrivateContext cReader
      emitGauge "h" h
      emitGauge "c" c
      emitCount "popped nodes"
      assign #openNodes newMap
      pure $ Just node
    Nothing -> pure Nothing

peekBest ::
  Ord n => Show node => Eq node => Hashable node => Monad m =>
  AstarT n node nodeStore pc w m (Maybe node)
peekBest = uses #openNodes $ fmap fst . PQueue.minView

valueNode :: Num n => Monad m => node -> AstarT n node nodeStore pc w m n
valueNode node = do
  hReader <- view #h <*> pure node
  cReader <- view #c <*> pure node
  runInPrivateContext $ (+) <$> hReader <*> cReader

astarMarkSeen :: Monad m => node -> AstarT n node nodeStore pc w m ()
astarMarkSeen node = do
  rememberNode <- view #rememberNode
  memory <- use #nodeStore
  newMemory <- runInPrivateContext $ rememberNode memory node
  assign #nodeStore newMemory

astarGetGoalNode :: Monad m => node -> AstarT n node nodeStore pc w m Bool
astarGetGoalNode node = view #isGoal <*> pure node >>= runInPrivateContext

astarExplodeNode :: Monad m => node -> AstarT n node nodeStore pc w m [node]
astarExplodeNode node = view #explode <*> pure node >>= runInPrivateContext

runInPrivateContext :: Monad m => Reader pc a -> AstarT n node nodeStore pc w m a
runInPrivateContext reader = runReader reader <$> view #privateContext

astarSeenNode :: Monad m => node -> AstarT n node nodeStore pc w m Bool
astarSeenNode node = do
  isSeen <- view #seenNode
  memory <- use #nodeStore
  runInPrivateContext (isSeen memory node)

astarPushNode ::
  Integral i => Num n => Ord n => Show node => MonadEmit (Metrics i n) m
  => node -> AstarT n node nodeStore pc w m ()
astarPushNode node = do
  value <- valueNode node
  emitCount "pushed nodes"
  modifying #openNodes $ PQueue.insert value node

-- For debugging. Making this an actual op will significantly slow down the run
-- trace :: Monad m => w -> AstarT node nodeMem pc w m ()
-- trace msg = tell (msg <> "\n")
