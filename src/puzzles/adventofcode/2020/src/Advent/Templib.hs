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
  listOfNum
  ) where

import           Advent.Perlude
import           Control.Lens               (view)
import           Control.Monad.Fail         (MonadFail)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader)
import           Control.Monad.Trans.Class  (MonadTrans)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Data.Functor.Identity      (Identity, runIdentity)
import           Data.Generics.Labels       ()
import           System.IO.Advent           (getInput)
import           Text.Parsec                (char, sepEndBy)
import           Text.Parsec.Text           (Parser)

import           Advent.Templib.Internal
import           Advent.Templib.Parsec      (digitsAsNum, unsafeParseAll)

data Day = D1
    | D2
    | D3
    | D4
    | D5
    | D6
    | D7
    | D8
    | D9
    | D10
    | D11
    | D12
    | D13
    | D14
    | D15
    | D16
    | D17
    | D18
    | D19
    | D20
    | D21
    | D22
    | D23
    | D24
    | D25
    deriving stock (Eq, Ord, Enum, Bounded, Show)

getInput' :: MonadIO m => Day -> m Text
getInput' = getInput . unpack . toText

toText :: Day -> Text
toText = show . (+ 1) . fromEnum

-- Get parsed input
getParsedInput :: MonadIO m => MonadFail m => Day -> Parser a -> m a
getParsedInput d p = getInput' d >>= unsafeParseAll p

-- Very common parsers
listOfNum :: Num n => Parser [n]
listOfNum = digitsAsNum `sepEndBy` char '\n'

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
