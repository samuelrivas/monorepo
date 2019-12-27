-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module Bidim (
  Bidim,
  Coord,
  boundaries,
  plus,
  showBindim
  ) where

import           Prelude         hiding (concat)

import           Control.Lens    (at, toListOf, traverse, view, _1, _2)
import           Data.Map.Strict (Map, keys)
import           Data.Text       (Text, concat, intercalate)

type Coord = (Int, Int)
type Bidim a = Map Coord a

-- Better would be to wrap this and make it an instance of Num
plus :: Coord -> Coord -> Coord
plus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- We need this to be a sorted map just to get the coordinate boundaries. This
-- can easily be improved if we wrap this in its own type and keep track of them
-- when inserting
showBindim :: (Maybe a -> Text) -> Bidim a -> Text
showBindim format bidim =
  let
    ((minX, minY), (maxX, maxY)) = boundaries bidim
    row y = (, y) <$> [minX..maxX]
    showCoord :: Coord -> Text
    showCoord (0, 0) = "$"
    showCoord (54, 34) = "%"
    showCoord coord = format $ view (at coord) bidim
    printed :: Int -> Text
    printed y = concat (showCoord <$> row y)
  in
    intercalate "\n" (printed <$> [minY..maxY])

boundaries :: Bidim a -> (Coord, Coord)
boundaries bidim =
  let
    coords = keys bidim
    xs = toListOf (traverse . _1) coords
    ys = toListOf (traverse . _2) coords
    maxX = maximum xs
    minX = minimum xs
    maxY = maximum ys
    minY = minimum ys
  in ((minX, minY), (maxX, maxY))

