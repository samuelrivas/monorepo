{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}

module Advent.Day23.Internal (
  ) where

import           Perlude
import qualified Prelude

import           Control.Monad             (forever)
import           Control.Monad.Reader      (MonadReader, ReaderT (runReaderT),
                                            ask)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Writer.CPS  (MonadWriter, WriterT, runWriterT,
                                            tell)
import           Data.Bidim                (Coord)
import           Data.Functor.Identity     (Identity, runIdentity)
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap
import           Data.HashSet              (HashSet)
import           Data.Hashable             (Hashable)
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.Text                 as Text
import           GHC.Generics              (Generic)
import           UnliftIO                  (MonadUnliftIO, TVar, atomically,
                                            modifyTVar, newTVar, newTVarIO,
                                            readTVar, readTVarIO, withAsync,
                                            writeTVar)
import           UnliftIO.Concurrent       (ThreadId, forkIO, threadDelay)

newtype Metrics a = Metrics { unMetrics :: HashMap Text a }
  deriving stock (Eq, Generic)
  deriving newtype (Functor, Foldable)

instance Semigroup a => Semigroup (Metrics a) where
  Metrics a <> Metrics b = Metrics $ HashMap.unionWith (<>) a b

instance Semigroup a => Monoid (Metrics a) where
  mappend = (<>)
  mempty = Metrics HashMap.empty

instance Show a => Show (Metrics a) where
  show =
    Text.unpack
    . Text.intercalate "\n"
    . HashMap.foldlWithKey' (\acc desc v -> (desc <> ": " <> show v) : acc) []
    . unMetrics

withMetricsPrinter ::
  MonadUnliftIO m => Show a => Int -> a -> (TVar a -> m b) -> m b
withMetricsPrinter delay initial x =
  do
    metrics <- newTVarIO initial
    withAsync (printPeriodically delay metrics) $ const (x metrics)

printPeriodically :: MonadIO m => Show a => Int ->  TVar a ->  m ()
printPeriodically delay metrics =
  forever $ readTVarIO metrics >>= print >> threadDelay delay

class (Monad m, Semigroup metric) => MonadEmit metric m | m -> metric where
  emit :: metric -> m ()

newtype EmitTVarT metric m a =
  EmitTVarT { unEmitTVarT :: ReaderT (TVar metric) m a }
  deriving newtype (Functor, Applicative, Monad, MonadReader (TVar metric),
                    MonadTrans, MonadIO)

type EmitTVar metric a = EmitTVarT metric Identity a

instance (Semigroup metrics, Monad m, MonadIO m) =>
  MonadEmit metrics (EmitTVarT metrics m) where
  emit metrics = do
    tvar <- ask
    atomically . modifyTVar tvar $ (metrics <>)

runEmitTVarT :: EmitTVarT metric m a -> TVar metric -> m a
runEmitTVarT  = runReaderT . unEmitTVarT

runEmitTVar :: EmitTVar metric a -> TVar metric -> a
runEmitTVar x = runIdentity . runEmitTVarT x

newtype EmitWriterT metric m a =
  EmitWriterT { unEmitWriterT :: WriterT metric m a }
  deriving newtype (Functor, Applicative, Monad, MonadWriter metric,
                    MonadTrans, MonadIO)

type EmitWriter metric a = EmitWriterT metric Identity a

instance (Monoid metrics, Monad m) =>
  MonadEmit metrics (EmitWriterT metrics m) where
  emit = tell

runEmitWriterT :: Monoid metric => EmitWriterT metric m a -> m (a, metric)
runEmitWriterT = runWriterT . unEmitWriterT

runEmitWriter :: Monoid metric => EmitWriter metric a -> (a, metric)
runEmitWriter = runIdentity . runEmitWriterT

-- Runs an emit monad with a listener thread that prints the collected metrics
-- periodically
runEmitTVarTIO :: Show metric => MonadUnliftIO m =>
  EmitTVarT metric m a -> Int -> metric -> m a
runEmitTVarTIO x delay initial = withMetricsPrinter delay initial (runEmitTVarT x)

runEmitTVarTIO' :: Show metric => Monoid metric => MonadUnliftIO m =>
  EmitTVarT metric m a -> Int ->  m a
runEmitTVarTIO' x delay = withMetricsPrinter delay mempty (runEmitTVarT x)
