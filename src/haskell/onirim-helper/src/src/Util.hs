-- Everything here is to be moved to a better named module
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Util
  (uncons,
   head,
   print,
   putStr,
   getLine,
   to_state,
   addHistory,
   readline
  )
where

import           Prelude                     hiding (getLine, head, print,
                                              putStr)
import qualified Prelude                     as Prelude

import           Control.Monad.Fail          (MonadFail)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.State.Lazy    (StateT (..))
import           Control.Monad.Trans.Maybe   (MaybeT (..))
import           Control.Monad.Writer        (WriterT (..))
import           Data.Random.Internal.Source (MonadRandom (getRandomPrim))
import qualified System.Console.Readline     as Readline

{-# ANN module "HLint: ignore Use camelCase" #-}

uncons :: (MonadFail m) => [a] -> m (a, [a])
uncons (x:xs) = return (x, xs)
uncons []     = fail "cannot uncons []"

head :: (MonadFail m) => [a] -> m a
head (x:_) = return x
head []    = fail "cannot head []"

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

-- XXX Why are these instance not there? And, heck, why are there two
-- MonadRandom implementations (there is an instance for
-- Control.Monad.Random.Class)

-- Fallback, this is quite explicit, but does work, delete once you make sure
-- that the more generic ones below also work
-- instance MonadIO m => MonadRandom (StateT s m) where
--   getRandomPrim = liftIO . getRandomPrimFrom DevURandom

instance MonadRandom m => MonadRandom (StateT s m) where
  getRandomPrim p = StateT $ \s ->
    do
      p' <- getRandomPrim p
      return (p', s)

instance MonadRandom m => MonadRandom (MaybeT m) where
  getRandomPrim p = MaybeT (Just <$> getRandomPrim p)

instance (Monoid w, MonadRandom m) => MonadRandom (WriterT w m) where
  getRandomPrim p = WriterT $ do
    p' <- getRandomPrim p
    return (p', mempty)

-- This may exist somewhere
to_state :: Monad m => ReaderT r m a -> StateT r m a
to_state r = StateT $ \s ->
  do
   a <- runReaderT r s
   return (a, s)
