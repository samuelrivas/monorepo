{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE LambdaCase                 #-}

module Control.Monad.MonadSearch (
  MonadSearch (..),
  search,
  step
  ) where

import           Control.Monad             (filterM, unless)

data SearchStatus node = Failed | Exploring | Found node
  deriving Show

class Monad m => MonadSearch node m | m -> node where
  popNode :: m (Maybe node)
  pushNode :: node -> m ()
  seenNode :: node -> m Bool
  goalNode :: node -> m Bool
  markSeen :: node -> m ()
  explode :: node -> m [node]

step :: MonadSearch node m => m (SearchStatus node)
step =
  popNode >>= \case
    Just node -> checkNode node
    Nothing -> pure Failed

checkNode :: MonadSearch node m => node -> m (SearchStatus node)
checkNode node =
  goalNode node >>= \case
    True -> pure $ Found node
    False -> do
      seen <- seenNode node
      unless seen $ explode node >>= pushNodes
      markSeen node
      pure Exploring

pushNodes :: MonadSearch node m => [node] -> m ()
pushNodes nodes = filterSeen nodes >>= mapM_ pushNode

-- Not necessary, but can save quite some memory
--
-- Todo evaluate the trad off between CPU and saved memory here
filterSeen :: MonadSearch node m => [node] -> m [node]
filterSeen = filterM (fmap not . seenNode)

search :: MonadSearch node m => m (Maybe node)
search = step >>= \case
  Found node -> pure $ Just node
  Failed -> pure Nothing
  Exploring -> search
