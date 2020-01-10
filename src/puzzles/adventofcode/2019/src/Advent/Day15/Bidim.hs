{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module Advent.Day15.Bidim (
  Coord,
  showMap
  ) where

import           Prelude         hiding (concat)

import           Control.Lens    (at, toListOf, traverse, view, _1, _2)
import           Data.Map.Strict (Map, keys)
import           Data.Text       (Text, concat, intercalate)

type Coord = (Int, Int)

-- We need this to be a sorted map just to get the coordinate boundaries. This
-- can easily be improved if we wrap this in its own type and keep track of them
-- when inserting
showMap :: (Maybe a -> Text) -> Map Coord a -> Text
showMap format plane =
  let
    coords = keys plane
    xs = toListOf (traverse . _1) coords
    ys = toListOf (traverse . _2) coords
    maxX = maximum xs
    minX = minimum xs
    maxY = maximum ys
    minY = minimum ys
    row y = (, y) <$> [minX..maxX]
    showCoord :: Coord -> Text
    showCoord (0,0) = "O"
    showCoord coord = format $ view (at coord) plane
    printed :: Int -> Text
    printed y = concat (showCoord <$> row y)
  in
    intercalate "\n" (printed <$> reverse [minY..maxY])
