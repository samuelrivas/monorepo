{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

-- These are not yet automated tests

module Main where

import           Perlude

import           Control.Lens               (view)
import           Control.Monad.MonadEmit    (EmitIdentity, EmitIdentityT (..),
                                             EmitState, EmitStateT (..),
                                             EmitTVarT (..),
                                             MetricsWrapper (..), MonadEmit,
                                             emitCount, emitGauge,
                                             runEmitIdentity, runEmitIdentityT,
                                             runEmitState, runEmitState',
                                             runEmitState'', runEmitStateT,
                                             runEmitStateT', runEmitStateT'',
                                             runEmitTVarT, runEmitTVarT',
                                             runEmitWriter, runEmitWriterT)
import           Control.Monad.Reader       (MonadReader, ReaderT (..),
                                             runReaderT)
import           Control.Monad.State.Strict (State, runState)
import           Data.Metrics.Internal      (Metrics)
import           GHC.Generics               (Generic)
import           UnliftIO.STM               (TVar, newTVarIO, readTVarIO)

increaseCount :: MonadEmit (Metrics Int Int)  m => m ()
increaseCount = emitCount "test count"

reportGauge :: MonadEmit (Metrics Int Int) m => m ()
reportGauge =
  let
    gauge = "test gauge"
  in
    emitGauge gauge 10 >> emitGauge gauge 20

-- Simulate that we are adding metrics capacity to an arbitrary application
-- monad wrapping ReaderT r IO
data ContextTVar metrics = ContextTVar {
  metricsTVar :: MetricsWrapper (TVar metrics),
  foo         :: Int
  } deriving (Generic)

newtype TVarApp metrics a = TVarApp (ReaderT (ContextTVar metrics) IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (ContextTVar metrics))
    via ReaderT (ContextTVar metrics) IO
  deriving (MonadEmit metrics)
    via EmitTVarT metrics (ContextTVar metrics) IO

runTVarApp :: TVarApp metrics a -> ContextTVar metrics -> IO a
runTVarApp (TVarApp reader) r = do
  putStrLn "running App"
  runReaderT reader r

-- Simulate that we are running tests over a mocked app that does not use IO
data ContextPure metrics = ContextPure {
  metricsValue :: MetricsWrapper metrics,
  bar          :: Int
  } deriving (Generic)

newtype PureApp metrics a = PureApp (State (ContextPure metrics) a)
  deriving (Functor, Applicative, Monad)
    via State (ContextPure metrics)
  deriving (MonadEmit metrics)
    via EmitState metrics (ContextPure metrics)

runPureApp ::
  PureApp metrics a -> ContextPure metrics -> (a, ContextPure metrics)
runPureApp (PureApp state) = runState state

pureEffect :: MonadEmit (Metrics Int Int) m => m ()
pureEffect = increaseCount >> reportGauge

ioEffect :: MonadIO m => MonadEmit (Metrics Int Int) m => Text -> m ()
ioEffect banner = pureEffect >> putStrLn banner

testRunEmitTVarT' :: IO ()
testRunEmitTVarT' =
  do
    tvar <- newTVarIO (mempty :: Metrics Int Int)
    runEmitTVarT' (ioEffect "test runEmitTVarT'") tvar
    readTVarIO tvar >>= print

mkTVarContext :: MonadIO m
  => m (ContextTVar (Metrics Int Int), TVar (Metrics Int Int))
mkTVarContext =
  do
    tvar <- newTVarIO (mempty :: Metrics Int Int)
    pure (ContextTVar (MetricsWrapper tvar) 42, tvar)

testRunEmitTVarT :: IO ()
testRunEmitTVarT =
  let
    effect =
      ioEffect "test runEmitTVarT"
      :: EmitTVarT (Metrics Int Int) (ContextTVar (Metrics Int Int)) IO ()
  in do
    (context, tvar) <- mkTVarContext
    runEmitTVarT effect context
    readTVarIO tvar >>= print

testRunTVarApp :: MonadIO m => m ()
testRunTVarApp =
  do
    (context, tvar) <- mkTVarContext
    liftIO $ runTVarApp (ioEffect "test runTVarApp") context
    readTVarIO tvar >>= print

testRunIdentityT :: forall m.MonadIO m => m ()
testRunIdentityT =
  runEmitIdentityT (ioEffect "test runIdentityT" :: EmitIdentityT (Metrics Int Int) m ())

-- This is to verify that we are not forced into IO
testRunPureIdentity :: MonadIO m => m ()
testRunPureIdentity =
  case runEmitIdentity (pureEffect :: EmitIdentity (Metrics Int Int) ())
  of () -> putStrLn "test testRunPureIdentity"

testRunWriterT :: MonadIO m => m ()
testRunWriterT =
  do
    ((), metrics) <- runEmitWriterT (ioEffect "test runEmitWriterT")
    print metrics

testRunWriter :: MonadIO m => m ()
testRunWriter =
  let ((), metrics) = runEmitWriter pureEffect
  in do
    putStrLn "test runEmitWriter"
    print metrics

testRunStateT :: forall m.MonadIO m => m ()
testRunStateT =
  let
    effect =
      ioEffect "test runEmitStateT"
      :: EmitStateT (Metrics Int Int) (MetricsWrapper (Metrics Int Int)) m ()
  in do
    ((), metrics) <- runEmitStateT effect $ MetricsWrapper mempty
    print metrics

testRunState :: MonadIO m => m ()
testRunState =
  let ((), metrics) =
        runEmitState
        (pureEffect :: EmitState (Metrics Int Int) (MetricsWrapper (Metrics Int Int)) ())
        (MetricsWrapper mempty)
  in do
    putStrLn "test runEmitState"
    print metrics

testRunStateT' :: MonadIO m => m ()
testRunStateT' =
  do
    ((), metrics) <- runEmitStateT' (ioEffect "test runEmitStateT'") mempty
    print metrics

testRunState' :: MonadIO m => m ()
testRunState' =
  let ((), metrics) = runEmitState' pureEffect mempty
  in do
    putStrLn "test runEmitState'"
    print metrics

testRunStateT'' :: MonadIO m => m ()
testRunStateT'' =
  do
    ((), metrics) <- runEmitStateT'' (ioEffect "test runEmitStateT''")
    print metrics

testRunState'' :: MonadIO m => m ()
testRunState'' =
  let ((), metrics) = runEmitState'' pureEffect
  in do
    putStrLn "test runEmitState''"
    print metrics

testRunPureApp :: MonadIO m => m ()
testRunPureApp =
  let
    context = ContextPure (MetricsWrapper (mempty :: Metrics Int Int)) 42
    ((), s) = runPureApp pureEffect context
  in do
    putStrLn "Test pure app"
    print $ view (#metricsValue . #unwrapMetric) s

main :: IO ()
main =
  do
    testRunEmitTVarT'
    testRunEmitTVarT
    testRunTVarApp

    testRunIdentityT
    putStrLn "test runIdentity"
    testRunPureIdentity

    testRunWriterT
    testRunWriter

    testRunStateT
    testRunState

    testRunStateT'
    testRunState'

    testRunStateT''
    testRunState''

    testRunPureApp
