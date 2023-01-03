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

-- TODO: Move this to Adventlib, these are here so that we don't need to keep
-- recompiling adventlib while experimenting

module Advent.Templib (
  solutions,
  input,
  runAdventT,
  runAdvent,
  AdventContext,
  Advent,
  MonadAdvent,
  AdventT (..),
  Day (..),
  getInput',
  getParsedInput,
  listOfNum,
  module Data.Advent,
  ) where

import           Perlude

import           Control.Lens               (view)
import           Control.Monad.Reader       (MonadReader)
import           Control.Monad.Trans.Class  (MonadTrans)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Data.Advent                (Day (..))
import           Data.Functor.Identity      (Identity, runIdentity)
import           Data.Generics.Labels       ()
import           System.IO.Advent           (getInput, getParsedInput)
import           Text.Parsec.Advent         (listOfNum)

import           Advent.Templib.Internal

-- Legacy, just for compatibility
getInput' :: MonadIO m => Day -> m Text
getInput' = getInput

-- Typeclass to encapsulate Advent problems.
--
-- Functions running in this monad have access to the parsed input
class Monad m =>  MonadAdvent c m | m -> c where
  input :: m c

-- Monad Transformer to add MonadAdvent functionality to any monad
newtype AdventT c m a = AdventT { unAdventT :: ReaderT (AdventContext c) m a }
  deriving newtype (Functor, Applicative, Monad, MonadReader (AdventContext c),
                    MonadTrans, MonadIO)

instance Monad m => MonadAdvent c (AdventT c m) where
   input = AdventT $ view #input

-- Unwrap AdventT to return the transformed monad
runAdventT :: AdventT c m a -> c -> m a
runAdventT x c = runReaderT (unAdventT x) (AdventContext c)

-- Simple monad to MonadAdvent functionality
type Advent c = AdventT c Identity

-- Run an Advent computation
runAdvent :: Advent c a -> c -> a
runAdvent x = runIdentity . runAdventT x

-- Given the solutions for part 1 and 2, print them
solutions ::
  MonadIO m => MonadAdvent c m => Show a => Show b =>
  m a -> m b -> m ()
solutions sol1 sol2 = do
  putStr "Solution 1: "
  sol1 >>= print

  putStr "Solution 2: "
  sol2 >>= print
