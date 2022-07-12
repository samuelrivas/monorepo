{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Monad.MonadEmit
  ( -- * MonadEmit class

  MonadEmit (emit),

  emitGauge,
  emitCount,
  emitCounts,

  -- * Wrapper to derive 'MonadEmit' using 'HasType'
  MetricsWrapper (..),

  -- * 'Identity' instances
  EmitIdentityT (..),
  EmitIdentity,
  runEmitIdentityT,
  runEmitIdentity,

  -- * Instances for 'Reader' with a TVar to store the metrics state
  EmitTVarT (..),
  EmitTVarT',
  EmitTVar,
  EmitTVar',
  runEmitTVarT,
  runEmitTVarT',
  runEmitTVarTWithPrinterThread,
  runEmitTVarTWithPrinterThread',

  -- * Instances for 'Writer'
  EmitWriterT (..),
  EmitWriter,
  runEmitWriterT,
  runEmitWriter,

  -- * Instances for 'Control.Monad.State.Strict' with a field to store the
  --   metrics state
  EmitStateT (..),
  EmitStateT',
  EmitState,
  EmitState',
  runEmitStateT,
  runEmitStateT',
  runEmitStateT'',
  runEmitState,
  runEmitState',
  runEmitState''
  ) where
import           Perlude

import           Control.Monad.MonadEmit.Internal

import           Control.Lens                     (_2, modifying, over, view)
import           Control.Monad.Identity           (Identity (..),
                                                   IdentityT (..), runIdentity,
                                                   runIdentityT)
import           Control.Monad.Reader             (MonadReader, ReaderT (..),
                                                   runReaderT)
import           Control.Monad.RWS.CPS            (tell)
import           Control.Monad.State              (MonadState)
import qualified Control.Monad.State.Lazy         as StateLazy
import qualified Control.Monad.State.Strict       as StateStrict
import           Control.Monad.Trans              (MonadTrans (lift))
import           Control.Monad.Writer.CPS         (MonadWriter, WriterT,
                                                   runWriterT)
import           Data.Generics.Labels             ()
import           Data.Generics.Product            (HasType, typed)
import qualified Data.HashMap.Strict              as HashMap
import           Data.Metrics
import           GHC.Generics                     (Generic)
import           UnliftIO                         (MonadUnliftIO, TVar,
                                                   atomically, modifyTVar)
-- TODO
-- Tidy up
-- Consider removing the instances on top. Do we need them?
-- Try to reduce wrappers to just one (the one we use for state)
-- Verify that all implementations are equivalent
-- A monad were we can emit metrics of type metric

class Monad m => MonadEmit metric m | m -> metric where
  emit :: metric -> m ()

emitGauge ::
  Integral i => Num n => MonadEmit (Metrics i n) m => Text -> n -> m ()
emitGauge name g =
  emit $ Metrics (HashMap.singleton name $ gaugeEntry g) HashMap.empty

emitCount :: Num i => MonadEmit (Metrics i n) m => Text -> m ()
emitCount name = emitCounts name 1

emitCounts :: Num i => MonadEmit (Metrics i n) m => Text -> i -> m ()
emitCounts name n =
  emit $ Metrics HashMap.empty (HashMap.singleton name (Count n))

-- * Instances for MTL monads
instance (Monad m, MonadEmit metrics m)
  => MonadEmit metrics (StateLazy.StateT s m) where
  emit = lift . emit

instance (Monad m, MonadEmit metrics m)
  => MonadEmit metrics (StateStrict.StateT s m) where
  emit = lift . emit

instance (Monad m, MonadEmit metrics m) => MonadEmit metrics (ReaderT r m) where
  emit = lift . emit

instance (Monad m, MonadEmit metrics m) => MonadEmit metrics (WriterT w m) where
  emit = lift . emit

-- * Wrapper type to instantiate 'MonadEmit' using 'HasType'
newtype MetricsWrapper metrics = MetricsWrapper { unwrapMetric :: metrics }
  deriving stock (Generic, Show, Eq)
  deriving (Semigroup, Monoid) via metrics

-- * Instances wrapping Identity
--
-- Identity monads ignore the 'emit' effect.
newtype EmitIdentityT metrics m a = EmitIdentityT
  { unEmitIdentityT :: IdentityT m a }
  deriving (Functor, Applicative, Monad, MonadReader r, MonadIO, MonadState s,
            MonadWriter w, MonadFail)
    via IdentityT m
  deriving (MonadTrans) via IdentityT

type EmitIdentity metrics = EmitIdentityT metrics Identity

instance (Monad m) => MonadEmit metrics (EmitIdentityT metrics m) where
  emit = const . pure $ ()

runEmitIdentityT :: EmitIdentityT metrics m a -> m a
runEmitIdentityT = runIdentityT . unEmitIdentityT

runEmitIdentity :: EmitIdentity metrics a -> a
runEmitIdentity = runIdentity . runEmitIdentityT

-- Emit monad that uses a TVar to send metrics
--
-- Newtype wrapping a reader with an arbitrary environment. This will have an
-- instance of MonadEmit if the reader contains a TVar

-- Tentative convention:
--
-- Wrapping type of name <Monad><Type>T, where Monad is the monad name and type
-- is the type for which we are creating the instance. As this wrapping type
-- typically wraps another transformer, the destructor is not to be used
-- directly, so is called un...
--
-- We write the non transformer type using Identity.
--
-- If we want to provide less polymorphic types as convenience, we add ', '',
-- ''' to them
--
-- For each of those, we create the equivalent non transformed types too
--
-- We provide run functions for each type, which unwrap the wrapping type and
-- the transformer it wraps (e.g. ReaderT in most cases)

newtype EmitTVarT metrics r m a =
  EmitTVarT { unEmitTVarT :: ReaderT r m a }
  deriving (Functor, Applicative, Monad, MonadReader r, MonadIO, MonadState s,
            MonadWriter w, MonadFail)
    via ReaderT r m
  deriving (MonadTrans) via ReaderT r

type EmitTVar metrics r = EmitTVarT metrics r Identity

type EmitTVarT' metrics = EmitTVarT metrics (MetricsWrapper (TVar metrics))
type EmitTVar' metrics = EmitTVarT metrics (MetricsWrapper (TVar metrics)) Identity

instance (Semigroup metrics,
          MonadIO m,
          HasType (MetricsWrapper (TVar metrics)) r) =>
  MonadEmit metrics (EmitTVarT metrics r m) where

  emit metrics = do
    tvar <- unwrapMetric <$> view typed
    atomically . modifyTVar tvar $ (metrics <>)

runEmitTVarT :: EmitTVarT metrics r m a -> r -> m a
runEmitTVarT = runReaderT . unEmitTVarT

runEmitTVarT' :: EmitTVarT' metrics m a -> TVar metrics -> m a
runEmitTVarT' x = runEmitTVarT x . MetricsWrapper

-- TODO Fix this so that it runs with the full EmitTVar type, but that probably
-- requires reworking withPrinterThread so that we can decouple the initial
-- environment from the TVar function ushing HasType

-- Runs an emit monad with a listener thread that prints the collected metrics
-- periodically
runEmitTVarTWithPrinterThread :: Show metrics => MonadUnliftIO m =>
  EmitTVarT' metrics m a -> Int -> metrics -> m a
runEmitTVarTWithPrinterThread x delay initial =
  withPrinterThread delay initial (runEmitTVarT' x)

-- Same 'runEmitTVarTWithPrinterThread', but using 'mempty' as initial value for
-- the metrics
runEmitTVarTWithPrinterThread' ::
  Monoid metrics
  => Show metrics
  => MonadUnliftIO m
  => EmitTVarT' metrics m a -> Int -> m a
runEmitTVarTWithPrinterThread' x delay =
  runEmitTVarTWithPrinterThread x delay mempty

-- Emit monad that collects metrics in a Writer
newtype EmitWriterT metrics m a =
  EmitWriterT { unEmitWriterT :: WriterT metrics m a }
  deriving (Functor, Applicative, Monad, MonadReader r, MonadIO, MonadState s,
            MonadWriter metrics, MonadFail)
    via WriterT metrics m
  deriving (MonadTrans) via WriterT metrics

type EmitWriter metrics a = EmitWriterT metrics Identity a

instance (Monoid metrics, Monad m) =>
  MonadEmit metrics (EmitWriterT metrics m) where
  emit = tell

runEmitWriterT :: Monoid metrics => EmitWriterT metrics m a -> m (a, metrics)
runEmitWriterT = runWriterT . unEmitWriterT

runEmitWriter :: Monoid metrics => EmitWriter metrics a -> (a, metrics)
runEmitWriter = runIdentity . runEmitWriterT

-- Emit monad that collects metrics in State
newtype EmitStateT metrics s m a =
  EmitStateT { unEmitStateT :: StateStrict.StateT s m a }
  deriving (Functor, Applicative, Monad, MonadReader r, MonadIO, MonadWriter w,
            MonadFail, MonadState s)
    via StateStrict.StateT s m
  deriving (MonadTrans) via StateStrict.StateT s

instance (Monad m, HasType (MetricsWrapper metrics) s, Semigroup metrics) =>
  MonadEmit metrics (EmitStateT metrics s m) where
  emit metric = modifying typed (MetricsWrapper metric <>)

type EmitState metrics s = EmitStateT metrics s Identity
type EmitStateT' metrics = EmitStateT metrics (MetricsWrapper metrics)
type EmitState' metrics = EmitState metrics (MetricsWrapper metrics)

runEmitStateT :: EmitStateT metrics s m a -> s -> m (a, s)
runEmitStateT = StateStrict.runStateT . unEmitStateT

runEmitState :: EmitState metrics s a -> s -> (a, s)
runEmitState x = runIdentity . runEmitStateT x

runEmitStateT' ::
  Functor m => EmitStateT' metrics m a -> metrics -> m (a, metrics)
runEmitStateT' x =
  fmap (over _2 unwrapMetric) . runEmitStateT x . MetricsWrapper

runEmitState' :: EmitState' metrics a -> metrics -> (a, metrics)
runEmitState' x = runIdentity . runEmitStateT' x

runEmitStateT'' ::
  Functor m
  => Monoid metrics
  => EmitStateT' metrics m a -> m (a, metrics)
runEmitStateT'' x = runEmitStateT' x mempty

runEmitState'' :: Monoid metrics => EmitState' metrics a -> (a, metrics)
runEmitState'' x = runEmitState' x mempty
