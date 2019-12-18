-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module MonadSearch (
  MonadSearch,
  search
  ) where

import           Control.Monad             (filterM)

data SearchStatus node = Failed | Exploring | Found node
  deriving Show

data NodeStatus = Seen | New | Goal
  deriving (Show, Eq)

class Monad m => MonadSearch node m | m -> node where
  popNode :: m (Maybe node)
  pushNode :: node -> m ()
  evalNode :: node -> m NodeStatus
  explode :: node -> m [node]

step :: MonadSearch node m => m (SearchStatus node)
step =
  popNode >>= \case
    Just node -> checkNode node
    Nothing -> pure Failed

checkNode :: MonadSearch node m => node -> m (SearchStatus node)
checkNode node =
  evalNode node >>= \case
    Goal -> pure $ Found node
    Seen -> pure Exploring
    New -> explode node >>= pushNodes >> pure Exploring

pushNodes :: MonadSearch node m => [node] -> m ()
pushNodes nodes = filterSeen nodes >>= mapM_ pushNode

-- Not necessary, but can save quite some memory
--
-- Todo evaluate the trad off between CPU and saved memory here
filterSeen :: MonadSearch node m => [node] -> m [node]
filterSeen = filterM (fmap (/= Seen) . evalNode)

search :: MonadSearch node m => m (Maybe node)
search = step >>= \case
  Found node -> pure $ Just node
  Failed -> pure Nothing
  Exploring -> search
