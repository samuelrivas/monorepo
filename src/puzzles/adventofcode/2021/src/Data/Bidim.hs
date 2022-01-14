{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}

module Data.Bidim (
  Bidim,
  Coord,
  boundaries,
  empty,
  insert,
  toMap,
  cross,
  fromText,
  plus,
  showBidim
  ) where

import           Prelude              hiding (concat)

import           Control.Lens         (_1, _2, at, over, set, toListOf,
                                       traverse, view)
import           Data.Foldable        (foldl')
import           Data.Generics.Labels ()
import qualified Data.Map             as Map
import           Data.Map.Strict      (Map)
import           Data.Text            (Text, concat, intercalate, unpack)
import           GHC.Generics         (Generic)

type Coord = (Int, Int)
data Bidim a = Bidim {
  toMap      :: Map Coord a,
  boundaries :: (Coord, Coord)
  } deriving stock (Show, Generic, Eq)

-- Better would be to wrap this and make it an instance of Num
plus :: Coord -> Coord -> Coord
plus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

empty :: Bidim a
empty = Bidim Map.empty (maxCoord, minCoord)

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
  over #toMap (Map.insert c a)
  . over #boundaries (extendBoundary c)

extendBoundary :: Coord -> (Coord, Coord) -> (Coord, Coord)
extendBoundary (x, y) ((minX, minY), (maxX, maxY)) =
  ((min minX x, min minY y), (max maxX x, max maxY y))

showBidim :: (Maybe a -> Text) -> Bidim a -> Text
showBidim format bidim =
  let
    ((minX, minY), (maxX, maxY)) = boundaries bidim
    row y = (, y) <$> [minX..maxX]
    showCoord :: Coord -> Text
    showCoord coord = format $ view (#toMap . at coord) bidim
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
