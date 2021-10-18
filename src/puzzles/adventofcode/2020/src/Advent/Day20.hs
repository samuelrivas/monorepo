{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day20 where

import           Perlude

import           Control.Lens     (at, both, each, foldlOf, non, over, view, _1,
                                   _2)
import           Control.Monad    (guard)
import           Data.Bidim       (Bidim, Coord, boundaries, fromText,
                                   showBidim)
import           Data.Hashable    (hash)
import           Data.IntMultiSet (IntMultiSet)
import qualified Data.IntMultiSet as IntMultiSet
import           Data.List        (find, foldl', sort, unfoldr)
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Maybe       (fromJust, fromMaybe, isJust)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import qualified Data.Text        as Text
import qualified System.IO.Advent as IOAdvent

-- Represent the edges of a tile, as they show as top edge in increasing
-- clockwise rotations.
-- That is
--
-- ...
-- ##.
-- #.#
--
-- Yields ("...", "##.", "#.#", "..#")
type Edges = (Text, Text, Text, Text)

type EdgeDb = Map Text (Set (Int,  Rotation))

-- Increasing clockwise rotations
data Rotation = R0
    | R1
    | R2
    | R3
    deriving stock (Show, Ord, Eq, Enum)

getInput :: IO Text
getInput = IOAdvent.getInput "20"

parse :: Text -> [(Int, Bidim Char)]
parse = fmap parseTile . Text.splitOn "\n\n" . Text.strip

parseTile :: Text -> (Int, Bidim Char)
parseTile text =
  let
    (idLine, tileT) = Text.breakOn "\n" text
    idText = fromJust $ Text.stripSuffix ":" idLine >>= Text.stripPrefix "Tile "
  in (read idText, fromText . Text.strip $ tileT)

showTile :: Bidim Char -> Text
showTile = (<> "\n") . showBidim (Text.singleton . fromMaybe '?')

tileToEdges :: Bidim Char -> [(Rotation, Text)]
tileToEdges bidim =
  let
    ((minX, minY), (maxX, maxY)) = boundaries bidim
    viewCoord coord = view (at coord . non '?') bidim
  in
    [(R0, Text.pack $ viewCoord <$> zip [minX..maxX] (repeat minY)),
     (R1, Text.pack $ viewCoord <$> zip (repeat minX) (reverse [minY..maxY])),
     (R2, Text.pack $ viewCoord <$> zip (reverse [minX..maxX]) (repeat maxY)),
     (R3, Text.pack $ viewCoord <$> zip (repeat maxX) [minY..maxY])]

-- Dictionary of all the available edges, mapped to their owning tile and the
-- rotation to put that edge on top
edgeDb :: [(Int, Bidim Char)] -> EdgeDb
edgeDb = foldl' insertTile Map.empty

insertTile :: EdgeDb -> (Int, Bidim Char) -> EdgeDb
insertTile db (tileId, tile) =
  let
    edges = tileToEdges tile
    toSignature (rotation, edge) = singleton edge (tileId, rotation)
  in
    foldl' union db (toSignature <$> edges)


-- TODO: Move this to utils
insert :: (Ord v, Ord k) => k -> v -> Map k (Set v) -> Map k (Set v)
insert k v = Map.insertWith Set.union k (Set.singleton v)

union :: (Ord v, Ord k) => Map k (Set v) -> Map k (Set v) -> Map k (Set v)
union = Map.unionWith Set.union

singleton :: (Ord v, Ord k) => k -> v -> Map k (Set v)
singleton k v = Map.singleton k (Set.singleton v)

-- Find matches for an id and edge. The left one is direct rotations, whereas
-- the second one is flipped rotations (i.e. when flipping the target tile)
findMatches :: EdgeDb -> Int -> Text -> (Set (Int, Rotation), Set (Int, Rotation))
findMatches db tileId edge =
  let
    removeSelf = Set.filter ((/= tileId) . view _1)
    direct = removeSelf . view (at edge . non Set.empty) $ db
    flipped = removeSelf . view (at (Text.reverse edge) . non Set.empty) $ db
  in
    (direct, flipped)

matchingTiles :: EdgeDb -> (Int, Bidim Char) -> Set Int
matchingTiles db (tileId, tile) =
  let
    edges = view _2 <$> tileToEdges tile
    matches = findMatches db tileId <$> edges
    merged = uncurry Set.union <$> matches
  in
    Set.map (view _1) . Set.unions $ merged

solution1 :: [(Int, Bidim Char)] -> Int
solution1 tiles =
  let
    db = edgeDb tiles
    numAdjacent = fmap Set.size $ matchingTiles db <$> tiles
    corners = filter ((== 2) . view _1) $ zip numAdjacent (view _1 <$> tiles)
  in
    product $ view _2 <$> corners

main :: IO ()
main = do
  input <- parse <$> getInput

  putStr "Solution 1: "
  print . solution1 $ input

  putStr "Solution 2: "
  print $ "NA"
