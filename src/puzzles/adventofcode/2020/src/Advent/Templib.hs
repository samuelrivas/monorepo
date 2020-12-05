{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO: Move this to Adventlib, these are here so that we don't need to keep
-- recompiling adventlib while experimenting

module Advent.Templib (
  solutions
  ) where

import Advent.Perlude

-- TODO: Monadify. The input should be in a MonadRead with the input, and an
-- arbitrary term in case we can share data between the solvers
--
-- solvers can also run in a kind of MonadLog to be able to report, or directly in the un restricted MonadIO
solutions :: MonadIO m => Show a => Show b => (Text -> a) -> (Text -> b) -> Text -> m ()
solutions solver1 solver2 input = do
  putStr "Solution 1: "
  print $ solver1 input

  putStr "Solution 2: "
  print $ solver2 input

