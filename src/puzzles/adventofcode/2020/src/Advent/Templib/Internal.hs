{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedLabels   #-}

module Advent.Templib.Internal (
  AdventContext (AdventContext)
  ) where

import           GHC.Generics (Generic)

-- We wrap this just in case we want to add more context in the future
newtype AdventContext a = AdventContext { input :: a }
    deriving stock Generic
