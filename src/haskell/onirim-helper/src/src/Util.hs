-- Everything here is to be moved to a better named module
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Util
  (uncons,
   head
  )
where

import           Control.Monad.Fail
import           Control.Monad.State.Lazy     hiding (fail)
import           Data.Random.Internal.Source  (MonadRandom (getRandomPrim),
                                               getRandomPrimFrom)
import           Data.Random.Source.DevRandom
import           Prelude                      hiding (fail, head)
{-# ANN module "HLint: ignore Use camelCase" #-}

uncons :: (MonadFail m) => [a] -> m (a, [a])
uncons (x:xs) = return (x, xs)
uncons []     = fail "cannot uncons []"

head :: (MonadFail m) => [a] -> m a
head (x:_) = return x
head []    = fail "cannot head []"

-- XXX Why is this instance not there? And, heck, why are there two MonadRandom
-- implementations (there is an instance for Control.Monad.Random.Class)
-- instance MonadRandom m => MonadRandom (StateT s m) where
instance MonadIO m => MonadRandom (StateT s m) where
  getRandomPrim = liftIO . getRandomPrimFrom DevURandom
