{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Advent.Day16.Internal (
  Rule (Rule),
  mkRule
  ) where

import           Perlude

import           GHC.Generics   (Generic)

data Rule = Rule
    { name   :: Text
    , ranges :: [(Int, Int)]
    }
    deriving stock (Generic, Eq, Show)

mkRule :: Text -> [(Int, Int)] -> Rule
mkRule = Rule

