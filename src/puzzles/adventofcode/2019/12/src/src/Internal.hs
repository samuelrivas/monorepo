-- This module exists mainly to keep accessors from leaking out of records

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedLabels   #-}
module Internal (Moon) where

import           Data.Generics.Labels ()
import Control.Lens (view)
import           GHC.Generics         (Generic)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

data Moon = Moon {
  pos :: (Integer, Integer, Integer),
  velocity :: (Integer, Integer, Integer)
  } deriving (Generic)

instance Show Moon where
  show a = "<pos=" <> (show . view #pos $ a)
    <> ", velocity=" <> (show . view #velocity $ a)
    <> ">"
