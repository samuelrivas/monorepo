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
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Astar where

import           Prelude

import           Control.Lens              (assign, at, modifying, uses, view)
import           Control.Monad.Fail        (MonadFail)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (Reader, runReader)
import           Control.Monad.RWS.CPS     (MonadReader, MonadState,
                                            MonadWriter, RWST, runRWST, tell)
import           Control.Monad.Trans.Class (MonadTrans)
import           Data.Generics.Labels      ()
import           Data.Hashable             (Hashable)
import qualified Data.HashSet              as HashSet
import qualified Data.Map.Strict           as Map
import Data.Text (Text, pack)

import           AstarInternal
import           MonadSearch

newtype AstarT node nodeMem pc w m a = AstarT {
  unAstarT :: RWST (AstarConfig node nodeMem  pc) w (AstarContext node nodeMem) m a
  } deriving newtype (Functor, Applicative, Monad, MonadWriter w,
                      MonadState (AstarContext node nodeMem),
                      MonadReader (AstarConfig node nodeMem pc), MonadIO,
                      MonadFail, MonadTrans)

instance
  (Show node,
   Eq node,
   Eq nodeMem,
   Hashable nodeMem,
   Hashable node,
   Monad m) =>
  MonadSearch node (AstarT node nodeMem pc Text m) where
  popNode = popBest
  pushNode = astarPushNode
  seenNode = astarSeenNode
  goalNode = astarGetGoalNode
  explode  = astarExplodeNode
  markSeen = astarMarkSeen

mkConfig ::
  (node -> Reader pc Int) -> -- h
  (node -> Reader pc Int) -> -- c
  (node -> Reader pc [node]) -> -- explode
  (node -> Reader pc Bool) -> -- isGoal
  (node -> Reader pc nodeMem) -> -- toMem
  pc ->
  AstarConfig node nodeMem pc
mkConfig = AstarConfig

runAstarT ::
  Monoid w
  => AstarT node nodeMem pc w m a
  -> AstarConfig node nodeMem pc
  -> AstarContext node nodeMem
  -> m (a, AstarContext node nodeMem, w)
runAstarT = runRWST . unAstarT

evalAstarT ::
  Show node =>
  Eq node =>
  Eq nodeMem =>
  Hashable node =>
  Hashable nodeMem =>
  Monad m =>
  AstarConfig node nodeMem pc -> node -> m (Maybe node, Text)
evalAstarT astarConfig initialNode  =
  let
    initialContext = AstarContext Map.empty HashSet.empty
  in do
    (nodeM, _, w) <- runAstarT
                       (astarPushNode initialNode >> search)
                       astarConfig
                       initialContext
    pure (nodeM, w)

evalAstarT' ::
  Show node => Eq node => Hashable node => Monad m =>
  AstarConfig node nodeMem pc -> node -> AstarT node nodeMem pc Text m a ->
  m (a, AstarContext node nodeMem, Text)
evalAstarT' astarConfig initialNode x =
  let initialContext = AstarContext Map.empty HashSet.empty
  in runAstarT
       (astarPushNode initialNode >> x)
       astarConfig
       initialContext

popBest ::
  Show node => Eq node => Hashable node => Monad m =>
  AstarT node nodeMem pc Text m (Maybe node)
popBest =
  uses #openNodes Map.minView >>= \case
    Just (node, newMap) -> do
      trace ("popping node: " <> pack (show node))
      assign #openNodes newMap
      pure $ Just node
    Nothing -> pure Nothing

valueNode :: Monad m => node -> AstarT node nodeMem pc w m Int
valueNode node = do
  hReader <- view #h <*> pure node
  cReader <- view #c <*> pure node
  runInPrivateContext $ (+) <$> hReader <*> cReader

astarMarkSeen ::
  Eq nodeMem => Hashable nodeMem => Monad m =>
  node -> AstarT node nodeMem pc w m ()
astarMarkSeen node = do
  toMem <- view #nodeToMem
  mem <- runInPrivateContext $ toMem node
  modifying #seenNodes (HashSet.insert mem)

astarGetGoalNode :: Monad m => node -> AstarT node nodeMem pc w m Bool
astarGetGoalNode node = view #isGoal <*> pure node >>= runInPrivateContext

astarExplodeNode :: Monad m => node -> AstarT node nodeMem pc w m [node]
astarExplodeNode node = view #explode <*> pure node >>= runInPrivateContext

runInPrivateContext :: Monad m => Reader pc a -> AstarT node nodeMem pc w m a
runInPrivateContext reader = runReader reader <$> view #privateContext

astarSeenNode ::
  Eq nodeMem => Hashable nodeMem => Monad m =>
  node -> AstarT node nodeMem pc w m Bool
astarSeenNode node = do
  toMem <- view #nodeToMem
  mem <- runInPrivateContext $ toMem node
  uses #seenNodes (HashSet.member mem)

astarPushNode :: Show node => Monad m => node -> AstarT node nodeMem pc Text m ()
astarPushNode node = do
  trace $ "Pushing node: " <> (pack . show $ node)
  value <- valueNode node
  assign (#openNodes . at value) . Just $ node

-- For debugging
trace :: Monad m => Text -> AstarT node nodeMem pc Text m ()
trace msg = tell (msg <> "\n")
