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

module Advent.Day24.Bidim (
  Bidim,
  Coord,
  boundaries,
  cross,
  fromText,
  plus,
  showBidim
  ) where

import           Prelude         hiding (concat)

import           Control.Lens    (at, set, toListOf, traverse, view, _1, _2)
import           Data.Foldable   (foldl')
import           Data.Map.Strict (Map, empty, insert, keys)
import           Data.Text       (Text, concat, intercalate, unpack)

type Coord = (Int, Int)
type Bidim a = Map Coord a

-- Better would be to wrap this and make it an instance of Num
plus :: Coord -> Coord -> Coord
plus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

cross :: Coord -> [Coord]
cross coord = [
  coord `plus` (1, 0),
  coord `plus` (0, 1),
  coord `plus` (-1, 0),
  coord `plus` (0, -1)
  ]

-- We need this to be a sorted map just to get the coordinate boundaries. This
-- can easily be improved if we wrap this in its own type and keep track of them
-- when inserting
showBidim :: (Maybe a -> Text) -> Bidim a -> Text
showBidim format bidim =
  let
    ((minX, minY), (maxX, maxY)) = boundaries bidim
    row y = (, y) <$> [minX..maxX]
    showCoord :: Coord -> Text
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

-- | Given an ascii representation of a bi-dimensional map, create a bidim where
-- cells are chars. For example
--
--  xyxyx
--  x x x
--  xyxyx
--
-- Creates a 5x3 Bidim, where cells are either 'x', 'y' or ' '
fromText :: Text -> Bidim Char
fromText out =
  let
    f (pos, bidim) '\n' = (set _1 0 pos `plus` (0, 1), bidim)
    f (pos, bidim) c    = (pos `plus` (1, 0), insert pos c bidim)
  in
    view _2 . foldl' f ((0 ,0), empty) $ unpack out
