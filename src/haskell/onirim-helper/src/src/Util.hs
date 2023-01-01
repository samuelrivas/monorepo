-- FIXME: Everything here is to be moved to a better named module
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Util
  (uncons,
   head,
   print,
   putStr,
   getLine,
   to_state,
   addHistory,
   readline,
   sampleIO,
   sampleRVarIO,
   last
  )
where

import           Prelude                  hiding (getLine, head, last, print,
                                           putStr)
import qualified Prelude

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Reader     (ReaderT, runReaderT)
import           Control.Monad.State.Lazy (StateT (..))
import           Data.Random              (Distribution, RVar, sampleFrom)
import qualified System.Console.Readline  as Readline
import           System.Random.Stateful   (globalStdGen)

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Explicitly unsafe operations
--
-- These are similar their original counterparts, but instead of being
-- implicitly unsafe they run in MonadFail so that we can use them explicitly
-- where it makes sense (or just because we are lazy, but honest :) )
uncons :: (MonadFail m) => [a] -> m (a, [a])
uncons (x:xs) = return (x, xs)
uncons []     = fail "cannot uncons []"

head :: (MonadFail m) => [a] -> m a
head (x:_) = return x
head []    = fail "cannot head []"

last :: (MonadFail m) => [a] -> m a
last l =
  case head . reverse $ l of
    (x:_) -> pure x
    _     -> fail "cannot last []"

-- Lifted IO
--
-- IO operations lifted to MonadIO
print :: (Show a, MonadIO m) => a -> m ()
print = liftIO . Prelude.print

putStr :: MonadIO m => String -> m ()
putStr = liftIO . Prelude.putStr

getLine :: MonadIO m => m String
getLine = liftIO Prelude.getLine

readline :: MonadIO m => String -> m (Maybe String)
readline = liftIO . Readline.readline

addHistory :: MonadIO m => String -> m ()
addHistory = liftIO . Readline.addHistory

-- Miscellaneous stuff
-- This may exist somewhere
to_state :: Monad m => ReaderT r m a -> StateT r m a
to_state r = StateT $ \s ->
  do
   a <- runReaderT r s
   return (a, s)

-- We are using sample in the wrong places, and it breaks with random-fu
sampleIO :: MonadIO m => Distribution d t => d t -> m t
sampleIO = sampleFrom globalStdGen

-- FIXME sampleFrom takes a Distribution rather than a Sampleable. RVars are
-- Sampleable but not Distribution, though any Distribution can be made an
-- RVar. The most reasonable interface is requiring Sampleable in sampleIO, but
-- I need to do some type jockeying to get there, and this hack gets the code to
-- compile again. A longer term solution requires rethinking the whole random
-- interface
sampleRVarIO :: MonadIO m => RVar t -> m t
sampleRVarIO = sampleFrom globalStdGen
