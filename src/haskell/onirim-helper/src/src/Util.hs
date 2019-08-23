-- Everything here is to be moved to a better named module
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Util
  (uncons,
   head,
   to_state
  )
where

import           Prelude                      hiding (head)

import           Control.Monad.Fail           (MonadFail)
import           Control.Monad.Reader         (ReaderT, runReaderT)
import           Control.Monad.State          (StateT)
import           Control.Monad.State.Lazy     (StateT (..))
import           Control.Monad.Trans.Maybe    (MaybeT (..))
import           Data.Random.Internal.Source  (MonadRandom (getRandomPrim))

{-# ANN module "HLint: ignore Use camelCase" #-}

uncons :: (MonadFail m) => [a] -> m (a, [a])
uncons (x:xs) = return (x, xs)
uncons []     = fail "cannot uncons []"

head :: (MonadFail m) => [a] -> m a
head (x:_) = return x
head []    = fail "cannot head []"

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

-- This may exist somewhere
to_state :: Monad m => ReaderT r m a -> StateT r m a
to_state r = StateT $ \s ->
  do
   a <- runReaderT r s
   return (a, s)
