{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TupleSections      #-}

module Data.Bidim (
  Bidim,
  Coord,
  empty,
  insert,
  toMap,
  cross,
  fromText,
  plus,
  showBidim,
  cell,
  singleton,
  coords,
  boundaries
  ) where

import           Prelude              hiding (concat)

import           Control.Lens         (Getter, Lens', _1, _2, at, lens, over,
                                       set, to, view)
import           Data.Foldable        (foldl')
import           Data.Generics.Labels ()
import qualified Data.Map             as Map
import           Data.Map.Strict      (Map)
import           Data.Text            (Text, concat, intercalate, unpack)

type Coord = (Int, Int)

-- We don't export a generic lens for this record, as the 'boundaries' field
-- depends on 'toMap'
data Bidim a = Bidim {
  toMap        :: Map Coord a,
  toBoundaries :: (Coord, Coord)
  } deriving stock (Show, Eq)

-- Do not export this lens, as the toMap field cannot be modified freely
asMap' :: Lens' (Bidim a) (Map Coord a)
asMap' = lens toMap (\b m -> b { toMap = m })

-- Do not export this lens, as the toBoundaries field cannot be modified freely
boundaries' :: Lens' (Bidim a) (Coord, Coord)
boundaries' = lens toBoundaries (\b m -> b { toBoundaries = m })

boundaries :: Getter (Bidim a) (Coord, Coord)
boundaries = to toBoundaries

coords :: Getter (Bidim a) [Coord]
coords = to (Map.keys . toMap)

-- Getter for the value of the Bidim in a given position
cell :: Coord -> Getter (Bidim a) (Maybe a)
cell c = to . view $ (asMap' . at c)

-- Better would be to wrap this and make it an instance of Num
plus :: Coord -> Coord -> Coord
plus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

empty :: Bidim a
empty = Bidim Map.empty (maxCoord, minCoord)

singleton :: Coord -> a -> Bidim a
singleton c a = Bidim (Map.singleton c a) (c, c)

-- TODO This is a bit of a hack, since we don't really need to start from empty
-- ever
maxCoord :: Coord
maxCoord = (maxBound, maxBound)

minCoord :: Coord
minCoord = (minBound, minBound)

cross :: Coord -> [Coord]
cross coord = [
  coord `plus` (1, 0),
  coord `plus` (0, 1),
  coord `plus` (-1, 0),
  coord `plus` (0, -1)
  ]

insert :: Coord -> a -> Bidim a -> Bidim a
insert c a =
  over asMap' (Map.insert c a)
  . over boundaries' (extendBoundary c)

extendBoundary :: Coord -> (Coord, Coord) -> (Coord, Coord)
extendBoundary (x, y) ((minX, minY), (maxX, maxY)) =
  ((min minX x, min minY y), (max maxX x, max maxY y))

showBidim :: (Maybe a -> Text) -> Bidim a -> Text
showBidim format bidim =
  let
    ((minX, minY), (maxX, maxY)) = toBoundaries bidim
    row y = (, y) <$> [minX..maxX]
    showCoord :: Coord -> Text
    showCoord coord = format $ view (asMap' . at coord) bidim
    printed :: Int -> Text
    printed y = concat (showCoord <$> row y)
  in
    intercalate "\n" (printed <$> [minY..maxY])

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
