-- This module exists mainly to keep accessors from leaking out of records

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedLabels   #-}

module Internal (
  Moon,
  Coord,
  mk_moon
) where

import           Data.Generics.Labels ()
import Control.Lens (view)
import           GHC.Generics         (Generic)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

type Coord = (Integer, Integer, Integer)

data Moon = Moon {
  pos :: Coord,
  velocity :: Coord
  } deriving (Generic)

instance Show Moon where
  show a = "<pos=" <> (show . view #pos $ a)
    <> ", vel=" <> (show . view #velocity $ a)
    <> ">"

mk_moon :: Coord -> Moon
mk_moon pos' = Moon pos' (0, 0, 0)
