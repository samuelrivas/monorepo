{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- These are not yet automated tests

module Main where

import           Perlude

import           Control.Monad.MonadEmit (MonadEmit, emitCount, emitGauge,
                                          runEmitTVarT, runEmitWriterT)
import           Data.Metrics            (Metrics)
import           UnliftIO.STM            (newTVarIO, readTVarIO)

increaseCount :: MonadEmit (Metrics Int Int)  m => m ()
increaseCount = emitCount "test count"

reportGauge :: MonadEmit (Metrics Int Int) m => m ()
reportGauge =
  let
    gauge = "test gauge"
  in
    emitGauge gauge 10 >> emitGauge gauge 20

main :: IO ()
main =
  do
    ((), metric) <- runEmitWriterT (increaseCount >> reportGauge)
    print metric

    metricTvar <- newTVarIO mempty
    () <- runEmitTVarT (increaseCount >> reportGauge) metricTvar
    readTVarIO metricTvar >>= print
