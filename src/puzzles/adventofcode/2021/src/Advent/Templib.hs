{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

module Advent.Templib (
  binToDec,
  bitString,
  bit,
  conv,
  linesOf,
  matrix,
  hPrint,
  hPutStr,
  solveM,
  MonadEmit (..),
  emitTVarT,
  emitTVar,
  EmitTVarT,
  runEmitTVarTIO,
  runEmitTVarTIO',
  EmitWriterT (..),
  emitWriter,
  runEmitWriter,
  runEmitWriterT,
  runEmitTVar,
  runEmitTVarT,
  EmitVoidT,
  EmitVoid,
  emitVoidT,
  emitVoid,
  runEmitVoid,
  runEmitVoidT,
  Metrics (..),
  gaugeEntry,
  emitGauge,
  emitCount,
  emitCounts
  ) where
import           Perlude

import qualified Prelude

import           Control.Applicative        ((<|>))
import           Control.Lens               (coerced, over, view, views)
import           Control.Monad              (forever)
import           Control.Monad.Identity     (Identity (..), IdentityT (..),
                                             runIdentity, runIdentityT)
import           Control.Monad.RWS.CPS      (tell)
import           Control.Monad.Reader       (MonadReader (ask, reader),
                                             ReaderT (..), runReaderT)
import           Control.Monad.State        (MonadState)
import qualified Control.Monad.State.Lazy   as StateLazy
import qualified Control.Monad.State.Strict as StateStrict
import           Control.Monad.Trans        (MonadTrans (lift))
import           Control.Monad.Writer.CPS   (MonadWriter, Writer, WriterT,
                                             runWriterT)
import           Data.Advent                (Day)
import           Data.Foldable              (foldl')
import           Data.Functor               (($>))
import           Data.Generics.Labels       ()
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.List                  (tails)
import           Data.Monoid                (Sum (..))
import qualified Data.Text                  as Text
import           GHC.Generics               (Generic)
import qualified System.IO                  as SIO
import           System.IO.Advent           (getParsedInput)
import           Text.Parsec                (char, many, many1, sepEndBy)
import           Text.Parsec.Parselib       (Parser, literal)
import           Text.Printf                (PrintfArg, printf)
import           UnliftIO                   (MonadUnliftIO, TVar, atomically,
                                             modifyTVar, newTVarIO, readTVarIO,
                                             stderr, withAsync)
import           UnliftIO.Concurrent        (threadDelay)

-- TODO use this in day 2 or delete
conv :: ([a] -> b) -> [a] -> [b]
conv f l = f <$> tails l

binToDec :: [Bool] -> Int
binToDec = foldl' (\acc b -> fromEnum b + 2*acc) 0

-- To perlude
hPrint :: MonadIO m => Show a => SIO.Handle -> a -> m ()
hPrint h = liftIO . SIO.hPrint h

hPutStr :: MonadIO m => SIO.Handle -> Text -> m ()
hPutStr h = liftIO . SIO.hPutStr h . unpack

hPutStrLn :: MonadIO m => SIO.Handle -> Text -> m ()
hPutStrLn h = liftIO . SIO.hPutStrLn h . unpack

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

data Metrics int num = Metrics {
  gauges :: HashMap Text (Gauge int num),
  counts :: HashMap Text (Count int)
  }
  deriving stock (Eq, Generic)

instance (Num n, Ord n, Integral i)
  => Semigroup (Metrics i n) where
  (<>) a = over #gauges (HashMap.unionWith (<>) (view #gauges a))
           . over #counts (HashMap.unionWith (<>) (view #counts a))

instance (Integral i, Num n, Ord n) => Monoid (Metrics i n) where
  mappend = (<>)
  mempty = Metrics HashMap.empty HashMap.empty

printMap :: Show a => HashMap Text a -> Text
printMap =
  Text.intercalate "\n"
  . HashMap.foldlWithKey' (\acc desc v -> (desc <> ": " <> show v) : acc) []

instance (Integral n, Fractional i, PrintfArg n, PrintfArg i, Show n, Show i)
  => Show (Metrics n i) where
  show ms =
    Text.unpack $ views #gauges printMap ms <> "\n" <> views #counts printMap ms

newtype Min n = Min { getMin :: n } deriving stock (Eq, Generic, Show)

instance Ord a => Semigroup (Min a) where
  Min a <> Min b = Min $ min a b

instance (Ord a, Bounded a) => Monoid (Min a) where
  mempty = Min maxBound

newtype Max n = Max { getMax :: n }
  deriving stock (Eq, Generic, Show)

instance Ord a => Semigroup (Max a) where
  Max a <> Max b = Max $ max a b

instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty = Max minBound

newtype Count n = Count { getCount :: n }
  deriving stock (Eq, Generic)
  deriving (Semigroup, Monoid) via (Sum n)

instance Show n => Show (Count n) where
  show c = Text.unpack $ "count " <> views #getCount show c

-- TODO move this to a private module to hide the accessors
data Gauge int num = Gauge {
  entries :: Count int,
  total   :: Sum num,
  max'    :: Max num,
  min'    :: Min num
  } deriving stock (Eq, Generic)

instance (Integral i, Fractional n, PrintfArg n, Show n, Show i)
  => Show (Gauge i n) where
  show = Text.unpack . showGauge

instance (Integral i, Num n, Ord n) => Semigroup (Gauge i n) where
  a <> b = Gauge {
    entries = view #entries a <> view #entries b,
    total = view #total a <> view #total b,
    max' = view #max' a <> view #max' b,
    min' = view #min' a <> view #min' b
    }

instance (Integral i, Num n, Bounded n, Ord n) => Monoid (Gauge i n) where
  mempty = Gauge {
    entries = mempty,
    total = mempty,
    max' = mempty,
    min' = mempty
    }
  mappend = (<>)

average :: forall n i.Integral i => Fractional n =>  Gauge i n -> n
average g =
  let
    es :: i = view (#entries . coerced) g
  in
    view (#total . coerced) g / fromIntegral es

-- view (#total . coerced) g / (view (#entries . coerced) g)

showGauge ::
  Integral i => Fractional n => PrintfArg n => Show n => Show i
  => Gauge i n -> Text
showGauge g =
    "(" <> views (#min' . #getMin) show g <> " | "
    <> Text.pack (printf "%.4f" $ average g)
    <> " | " <> views (#max' . #getMax) show g <> ") "
    <> views #total (show . getSum) g <> "/"
    <> views (#entries . #getCount) show g

gaugeEntry :: Num n => Integral i => n -> Gauge i n
gaugeEntry n = Gauge (Count 1) (Sum n) (Max n) (Min n)

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
printPeriodically delay tvar =
  forever $
  readTVarIO tvar >>= hPrint stderr >> hPutStrLn stderr "" >> threadDelay delay

-- A monad were we can emit metrics of type a
class Monad m => MonadEmit metric m | m -> metric where
  emit :: metric -> m ()

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

-- Emit monad that uses a TVar to send metrics
newtype EmitTVarT metric m a =
  EmitTVarT { unEmitTVarT :: ReaderT (TVar metric) m a }
  deriving newtype (Functor, Applicative, Monad, MonadReader (TVar metric),
                    MonadTrans, MonadIO, MonadState s, MonadFail, MonadWriter w)

type EmitTVar metric a = EmitTVarT metric Identity a

-- instance MonadState s m => MonadState s (EmitTVarT metrics m) where
--   state = undefined

-- instance MonadEmit metrics m => MonadEmit metrics (EmitTVarT metrics' m) where
--   emit = undefined

instance (Semigroup metrics, Monad m, MonadIO m) =>
  MonadEmit metrics (EmitTVarT metrics m) where
  emit metrics = do
    tvar <- ask
    atomically . modifyTVar tvar $ (metrics <>)

emitTVarT :: (TVar metric -> m a) -> EmitTVarT metric m a
emitTVarT = EmitTVarT . ReaderT

emitTVar :: (TVar metric -> a) -> EmitTVar metric a
emitTVar = EmitTVarT . reader

runEmitTVarT :: EmitTVarT metric m a -> TVar metric -> m a
runEmitTVarT  = runReaderT . unEmitTVarT

runEmitTVar :: EmitTVar metric a -> TVar metric -> a
runEmitTVar x = runIdentity . runEmitTVarT x

-- Runs an emit monad with a listener thread that prints the collected metrics
-- periodically
runEmitTVarTIO :: Show metric => MonadUnliftIO m =>
  EmitTVarT metric m a -> Int -> metric -> m a
runEmitTVarTIO x delay initial = withPrinterThread delay initial (runEmitTVarT x)

-- Same as runEmitTVarTIO, but using 'mempty' as initial value for the metrics
runEmitTVarTIO' :: Show metric => Monoid metric => MonadUnliftIO m =>
  EmitTVarT metric m a -> Int ->  m a
runEmitTVarTIO' x delay = withPrinterThread delay mempty (runEmitTVarT x)

-- Emit monad that collects metrics in a Writer
newtype EmitWriterT metric m a =
  EmitWriterT { unEmitWriterT :: WriterT metric m a }
  deriving newtype (Functor, Applicative, Monad, MonadWriter metric,
                    MonadTrans, MonadIO)

type EmitWriter metric a = EmitWriterT metric Identity a

-- instance MonadState s m => MonadState s (EmitWriterT metric m) where
--   state = undefined

emitWriter :: Writer metric a -> EmitWriter metric a
emitWriter = EmitWriterT

instance (Monoid metric, Monad m) =>
  MonadEmit metric (EmitWriterT metric m) where
  emit = tell

runEmitWriterT :: Monoid metric => EmitWriterT metric m a -> m (a, metric)
runEmitWriterT = runWriterT . unEmitWriterT

runEmitWriter :: Monoid metric => EmitWriter metric a -> (a, metric)
runEmitWriter = runIdentity . runEmitWriterT

-- Emit monad that discards metrics
--
-- This is useful to unwrap Emit monads without caring about the metrics:
--
-- runEmitVoidT (emit (Sum 10)) :: Monad m => m ()

newtype EmitVoidT metric m a = EmitVoidT { unEmitVoidT :: IdentityT m a }
  deriving newtype (Functor, Applicative, MonadTrans, MonadIO, Monad)

type EmitVoid metric a = EmitVoidT metric Identity a

emitVoidT :: m a -> EmitVoidT metric m a
emitVoidT = EmitVoidT . IdentityT

emitVoid :: a -> EmitVoid metric a
emitVoid = EmitVoidT . IdentityT . Identity

instance (Semigroup metric, Monad m) =>
  MonadEmit metric (EmitVoidT metric m) where
  emit = const . pure $ ()

-- instance MonadState s m => MonadState s (EmitVoidT metric m) where
--   state = undefined

runEmitVoidT :: EmitVoidT metric m a -> m a
runEmitVoidT = runIdentityT . unEmitVoidT

runEmitVoid :: EmitVoid metric a -> a
runEmitVoid = runIdentity . runEmitVoidT

-- Helper functions to emit Metrics
emitGauge ::
  Integral i => Num n => MonadEmit (Metrics i n) m
  => Text -> n -> m ()
emitGauge name g =
  emit $ Metrics (HashMap.singleton name $ gaugeEntry g) HashMap.empty

emitCount :: Num i => MonadEmit (Metrics i n) m => Text -> m ()
emitCount name = emitCounts name 1

emitCounts :: Num i => MonadEmit (Metrics i n) m => Text -> i -> m ()
emitCounts name n =
  emit $ Metrics HashMap.empty (HashMap.singleton name (Count n))
