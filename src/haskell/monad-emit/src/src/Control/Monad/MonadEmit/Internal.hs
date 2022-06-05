{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.MonadEmit.Internal (
  withPrinterThread
  ) where

import           Perlude


import           Control.Monad           (forever)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Data.Generics.Labels    ()
import           UnliftIO                (newTVarIO)
import           UnliftIO.Async          (withAsync)
import           UnliftIO.Concurrent     (threadDelay)
import           UnliftIO.IO             (stderr)
import           UnliftIO.STM            (TVar, readTVarIO)

-- IO Helpers

-- Run a thread that prints the contents of a TVar periodically, and pass that
-- TVar to another computation so that it can modify its contents. This is
-- useful to print progress of a long computation
withPrinterThread ::
  MonadUnliftIO m => Show a => Int -> a -> (TVar a -> m b) -> m b
withPrinterThread delay initial x =
  do
    metrics <- newTVarIO initial
    withAsync (printPeriodically delay metrics) $ const (x metrics)

-- Parsers
-- Print the contents of a TVar periodically, forever
printPeriodically :: MonadIO m => Show a => Int -> TVar a -> m ()
printPeriodically delay tvar =
  forever $
  readTVarIO tvar >>= hPrint stderr >> hPutStrLn stderr "" >> threadDelay delay
