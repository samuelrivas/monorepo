{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Advent.Templib (
  binToDec,
  bitString,
  bit,
  conv,
  linesOf,
  matrix,
  hPrint,
  solveM,
  runEmitVoid,
  runEmitVoidT,
  runEmitTVarTIO,
  runEmitWriter,
  runEmitWriterT,
  runEmitTVar,
  runEmitTVarT,
  MonadEmit (..),
  Metrics (..)
  ) where
import           Perlude

import qualified Prelude

import           Control.Applicative      ((<|>))
import           Control.Monad            (forever)
import           Control.Monad.Identity   (Identity (..), IdentityT (..),
                                           runIdentity, runIdentityT)
import           Control.Monad.RWS.CPS    (tell)
import           Control.Monad.Reader     (MonadReader (ask), ReaderT,
                                           runReaderT)
import           Control.Monad.Trans      (MonadTrans)
import           Control.Monad.Writer.CPS (MonadWriter, WriterT, runWriterT)
import           Data.Advent              (Day)
import           Data.Foldable            (foldl')
import           Data.Functor             (($>))
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HashMap
import           Data.List                (tails)
import           Data.Monoid              (Sum)
import qualified Data.Text                as Text
import           GHC.Generics             (Generic)
import qualified System.IO                as SIO
import           System.IO.Advent         (getParsedInput)
import           Text.Parsec              (char, many, many1, sepEndBy)
import           Text.Parsec.Parselib     (Parser, literal)
import           UnliftIO                 (MonadUnliftIO, TVar, atomically,
                                           modifyTVar, newTVarIO, readTVarIO,
                                           stderr, withAsync)
import           UnliftIO.Concurrent      (threadDelay)

-- TODO use this in day 2 or delete
conv :: ([a] -> b) -> [a] -> [b]
conv f l = f <$> tails l

binToDec :: [Bool] -> Int
binToDec = foldl' (\acc b -> fromEnum b + 2*acc) 0

-- To perlude
hPrint :: MonadIO m => Show a => SIO.Handle -> a -> m ()
hPrint h = liftIO . SIO.hPrint h

-- Parsers

linesOf :: Parser a -> Parser [a]
linesOf p = p `sepEndBy` char '\n'

bit :: Parser Bool
bit = (literal "1" $> True) <|> (literal "0" $> False)

bitString :: Parser [Bool]
bitString = many1 bit

matrix :: Parser a -> Parser [[a]]
matrix p =
  let cell = many (char ' ') *> p
  in many1 cell `sepEndBy` char '\n'

-- Advent templates
solveM ::
  MonadFail m =>
  MonadIO m =>
  MonadUnliftIO m =>
  Monoid metrics1 =>
  Monoid metrics2 =>
  Show metrics1 =>
  Show metrics2 =>
  Show a =>
  Show b =>
  Day ->
  Parser input ->
  (input -> EmitTVarT metrics1 m a) ->
  (input -> EmitTVarT metrics2 m b) ->
  m ()
solveM day parser solver1 solver2 = do
  input <- getParsedInput day parser

  solution1 <- runEmitTVarTIO' (solver1 input) 1000000
  putStr "Solution 1: "
  print solution1

  solution2 <- runEmitTVarTIO' (solver2 input) 1000000
  putStr "Solution 2: "
  print solution2

-- Monad Emit

-- A monad were we can emit metrics of type a
newtype Metrics a = Metrics { unMetrics :: HashMap Text a }
  deriving stock (Eq, Generic)
  deriving newtype (Functor, Foldable)

-- The default instances for HashMap aren't really monoidal...
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

-- Run a thread that prints the contents of a TVar periodically, and pas that
-- TVar to another computation so that it can modify its contents. This is
-- useful to print progress of a long computation
withPrinterThread ::
  MonadUnliftIO m => Show a => Int -> a -> (TVar a -> m b) -> m b
withPrinterThread delay initial x =
  do
    metrics <- newTVarIO initial
    withAsync (printPeriodically delay metrics) $ const (x metrics)

-- Print the contents of a TVar periodically, forever
printPeriodically :: MonadIO m => Show a => Int -> TVar a -> m ()
printPeriodically delay metrics =
  forever $ readTVarIO metrics >>= hPrint stderr >> threadDelay delay

class (Monad m, Semigroup metric) => MonadEmit metric m | m -> metric where
  emit :: metric -> m ()

-- Instances for Identity, to run Emit actions discarding the metrics. Note that
-- we cannot derive this for all emittable types, since Identity doesn't
-- determine the type, so if you do emit any type you cannot use this shortcut
-- and need to use a EmitVoid
--
-- runIdentity $ emit () :: ()
-- runIdentityT $ emit () :: Monad f => f ()
instance MonadEmit () Identity where
  emit = const . Identity $ ()

instance (Monad m) => MonadEmit () (IdentityT m) where
  emit = const . IdentityT . pure $ ()

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

instance (Monoid metric, Monad m) =>
  MonadEmit metric (EmitWriterT metric m) where
  emit = tell

runEmitWriterT :: Monoid metric => EmitWriterT metric m a -> m (a, metric)
runEmitWriterT = runWriterT . unEmitWriterT

runEmitWriter :: Monoid metric => EmitWriter metric a -> (a, metric)
runEmitWriter = runIdentity . runEmitWriterT

newtype EmitVoidT metric m a = EmitVoidT { unEmitVoidT :: IdentityT m a }
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

type EmitVoid metric a = EmitVoidT metric Identity a

instance (Semigroup metric, Monad m) =>
  MonadEmit metric (EmitVoidT metric m) where
  emit = const . EmitVoidT $ pure ()

runEmitVoidT :: EmitVoidT metric m a -> m a
runEmitVoidT = runIdentityT . unEmitVoidT

runEmitVoid :: EmitVoid metric a -> a
runEmitVoid = runIdentity . runEmitVoidT

-- Runs an emit monad with a listener thread that prints the collected metrics
-- periodically
runEmitTVarTIO :: Show metric => MonadUnliftIO m =>
  EmitTVarT metric m a -> Int -> metric -> m a
runEmitTVarTIO x delay initial = withPrinterThread delay initial (runEmitTVarT x)

runEmitTVarTIO' :: Show metric => Monoid metric => MonadUnliftIO m =>
  EmitTVarT metric m a -> Int ->  m a
runEmitTVarTIO' x delay = withPrinterThread delay mempty (runEmitTVarT x)
