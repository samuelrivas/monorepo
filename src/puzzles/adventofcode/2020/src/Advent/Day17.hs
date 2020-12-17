{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day17 where

import           Advent.Perlude

import           Control.Lens        (at, both, each, foldlOf, modifying, non,
                                      over, set, toListOf, use, view, _1, _2,
                                      _3)
import           Control.Monad       (filterM, guard, replicateM_)
import           Control.Monad.State (MonadState, evalState, get, gets, put)
import           Data.Bidim          (Bidim, fromText, showBidim)
import           Data.List           (find, foldl', sort, unfoldr)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (fromJust, isJust)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import qualified Data.Text           as Text
import qualified System.IO.Advent    as IOAdvent

example :: Text
example = ".#.\n\
          \..#\n\
          \###\n"

type Coord = (Int, Int, Int)
type Cells = Map Coord Bool

-- TODO: We can cut this in half since the solutions are symmetric for z. (x, y,
-- z) == (x, y, -z)

-- TODO: Generalise coord

-- We only keep active cells here. Just in case the cells are bools, but for
-- part one at least they could just be ()
parse :: Text -> Cells
parse =
  Map.filter id
  . Map.mapKeys (\(x, y) -> (x, y, 0))
  . Map.map (== '#')
  . fromText

showCells :: Int -> Cells -> Text
showCells depth cells =
  let
    bidim :: Bidim Bool = Map.mapKeys (\(x, y, _) -> (x, y))
                          . Map.filterWithKey (\(_, _, z) _ -> z == depth)
                          $ cells
    showCell (Just True) = "#"
    showCell _           = "."
  in
    showBidim showCell bidim

-- TODO: Proudly copied from Bidim
boundaries :: Cells -> (Coord, Coord)
boundaries cells =
  let
    coords = Map.keys cells
    xs = toListOf (traverse . _1) coords
    ys = toListOf (traverse . _2) coords
    zs = toListOf (traverse . _3) coords
    maxX = maximum xs
    minX = minimum xs
    maxY = maximum ys
    minY = minimum ys
    maxZ = maximum zs
    minZ = minimum zs
  in ((minX, minY, minZ), (maxX, maxY, maxZ))

-- Return all coords that need to be updated
allCoords :: Cells -> [Coord]
allCoords cells =
  let ((minX, minY, minZ), (maxX, maxY, maxZ)) = boundaries cells
  in do
    x <- [minX - 1..maxX + 1]
    y <- [minY - 1..maxY + 1]
    z <- [minZ - 1..maxZ + 1]
    pure (x, y, z)

neighbours :: Coord -> [Coord]
neighbours coord@(x, y, z) = filter (/= coord) $ do
  newX <- [x - 1..x + 1]
  newY <- [y - 1..y + 1]
  newZ <- [z - 1..z + 1]
  pure (newX, newY, newZ)

step :: MonadState Cells m => m ()
step = do
  current <- get
  toSet <- newActiveCoords
  toUnset <- newInactiveCoords
  put $ Map.difference (Map.union toSet current) toUnset

getActive :: MonadState Cells m => m Int
getActive = gets $ length . Map.keys

newActiveCoords :: MonadState Cells m => m Cells
newActiveCoords = do
  inactiveCoords <- gets allCoords >>= filterCellState False
  toActivate <- filterM mustActivate inactiveCoords
  pure . Map.fromList . zip toActivate $ repeat True

newInactiveCoords :: MonadState Cells m => m Cells
newInactiveCoords =  do
  activeCoords <- gets allCoords >>= filterCellState True
  toDeactivate <- filterM mustDeactivate activeCoords
  pure . Map.fromList . zip toDeactivate $ repeat True

filterCellState :: MonadState Cells m => Bool -> [Coord] -> m [Coord]
filterCellState active = filterM (fmap (== active) . isActive)

isActive :: MonadState Cells m => Coord -> m Bool
isActive coord = use (at coord . non False)

mustActivate :: MonadState Cells m => Coord -> m Bool
mustActivate = fmap ((== 3) . length) . filterCellState True . neighbours

mustDeactivate :: MonadState Cells m => Coord -> m Bool
mustDeactivate = fmap (not . (`elem` [2, 3]) . length) . filterCellState True . neighbours

getInput :: IO Text
getInput = IOAdvent.getInput "17"

solve1 :: Cells -> Int
solve1 = evalState (replicateM_ 6 step >> getActive)

main :: IO ()
main = do
  input <- parse <$> getInput

  putStr "Solution 1: "
  print . solve1 $ input

  putStr "Solution 2: "
  print $ "NA"
