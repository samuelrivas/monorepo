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

import           Advent.Templib                          (Metrics,
                                                          MonadEmit (..),
                                                          emitCount, emitGauge)
import           Control.Lens                            (assign, modifying,
                                                          use, uses, view)
import           Control.Monad.MonadSearch               (MonadSearch (..),
                                                          search)
import           Control.Monad.RWS.CPS                   (MonadReader,
                                                          MonadState,
                                                          MonadWriter, RWST,
                                                          runRWST)
import           Control.Monad.Reader                    (Reader, runReader)
import           Control.Monad.Trans.Class               (MonadTrans, lift)
import           Data.Generics.Labels                    ()
import qualified Data.HashSet                            as HashSet
import           Data.Hashable                           (Hashable)
import qualified Data.PriorityQueue.FingerTree           as PQueue

import           Control.Monad.MonadSearch.AstarInternal
import           Data.HashSet                            (HashSet)

newtype AstarT n node nodeMem pc w m a = AstarT {
  unAstarT :: RWST (AstarConfig n node nodeMem pc) w (AstarContext n node nodeMem) m a
  } deriving newtype (Functor, Applicative, Monad, MonadWriter w,
                      MonadState (AstarContext n node nodeMem),
                      MonadReader (AstarConfig n node nodeMem pc), MonadIO,
                      MonadFail, MonadTrans)

instance (MonadEmit metrics m) =>
  MonadEmit metrics (AstarT n node nodeMem pc w m) where
  emit = lift . emit

instance
  (Show node,
   Eq node,
   Eq nodeMem,
   Hashable nodeMem,
   Hashable node,
   Integral i,
   Num n,
   Ord n,
   MonadEmit (Metrics i n) m) =>
  MonadSearch node (AstarT n node nodeMem pc w m) where
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
  (HashSet nodeMem -> node -> Reader pc (HashSet nodeMem)) -> -- rememberNode
  (HashSet nodeMem -> node -> Reader pc Bool) -> -- rememberNode
  pc ->
  AstarConfig n node nodeMem pc
mkConfig = AstarConfig

runAstarT ::
  Monoid w
  => AstarT n node nodeMem pc w m a
  -> AstarConfig n node nodeMem pc
  -> AstarContext n node nodeMem
  -> m (a, AstarContext n node nodeMem, w)
runAstarT = runRWST . unAstarT

searchAstarT ::
  Num n =>
  Integral i =>
  Ord n =>
  Monoid w =>
  Show node =>
  Eq node =>
  Eq nodeMem =>
  Hashable node =>
  Hashable nodeMem =>
  MonadEmit (Metrics i n) m =>
  AstarConfig n node nodeMem pc -> node -> m (Maybe node, w)
searchAstarT astarConfig initialNode  = do
  (nodeM, _, w) <- runInAstarT search astarConfig initialNode
  pure (nodeM, w)

runInAstarT ::
  Integral i => Num n => Ord n => Monoid w => Show node => Eq node
  => Hashable node
  => MonadEmit (Metrics i n) m
  => AstarT n node nodeMem pc w m a -> AstarConfig n node nodeMem pc -> node ->
  m (a, AstarContext n node nodeMem, w)
runInAstarT x astarConfig initialNode =
  let initialContext = AstarContext PQueue.empty HashSet.empty
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
  => AstarT n node nodeMem pc w m (Maybe node)
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
  AstarT n node nodeMem pc w m (Maybe node)
peekBest = uses #openNodes $ fmap fst . PQueue.minView

valueNode :: Num n => Monad m => node -> AstarT n node nodeMem pc w m n
valueNode node = do
  hReader <- view #h <*> pure node
  cReader <- view #c <*> pure node
  runInPrivateContext $ (+) <$> hReader <*> cReader

astarMarkSeen ::
  Eq nodeMem => Hashable nodeMem => Monad m =>
  node -> AstarT n node nodeMem pc w m ()
astarMarkSeen node = do
  rememberNode <- view #rememberNode
  memory <- use #nodeMemory
  newMemory <- runInPrivateContext $ rememberNode memory node
  assign #nodeMemory newMemory

astarGetGoalNode :: Monad m => node -> AstarT n node nodeMem pc w m Bool
astarGetGoalNode node = view #isGoal <*> pure node >>= runInPrivateContext

astarExplodeNode :: Monad m => node -> AstarT n node nodeMem pc w m [node]
astarExplodeNode node = view #explode <*> pure node >>= runInPrivateContext

runInPrivateContext :: Monad m => Reader pc a -> AstarT n node nodeMem pc w m a
runInPrivateContext reader = runReader reader <$> view #privateContext

astarSeenNode ::
  Eq nodeMem => Hashable nodeMem => Monad m
  => node -> AstarT n node nodeMem pc w m Bool
astarSeenNode node = do
  isSeen <- view #seenNode
  memory <- use #nodeMemory
  runInPrivateContext (isSeen memory node)

astarPushNode ::
  Integral i => Num n => Ord n => Show node => MonadEmit (Metrics i n) m
  => node -> AstarT n node nodeMem pc w m ()
astarPushNode node = do
  value <- valueNode node
  emitCount "pushed nodes"
  modifying #openNodes $ PQueue.insert value node

-- For debugging. Making this an actual op will significantly slow down the run
-- trace :: Monad m => w -> AstarT node nodeMem pc w m ()
-- trace msg = tell (msg <> "\n")
