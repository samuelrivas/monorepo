{-# LANGUAGE NoImplicitPrelude #-}

-- TODO: Move this to a lib, these are here so that we don't need to keep
-- recompiling adventlib while experimenting

module Advent.Templib.Bool (
  xor
  ) where

import           Advent.Perlude

-- Logic exclusive or
xor :: Bool -> Bool -> Bool
xor True True   = False
xor False False = False
xor _ _         = True

