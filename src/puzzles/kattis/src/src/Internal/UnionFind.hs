-- TODO
-- Generalise types to MArray and Array if reasonable
-- Generalise over ST and IO
-- Convert into opaque monad
-- Extract tests
-- Union by rank to prevent unbalanced trees
-- Add a print tree for debugging
-- Generalise over Ix, that may help with bidimensional representations
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Internal.UnionFind (
  find,
  find',
  new,
  toArray,
  toUnionFind,
  toText,
  union,

  MutableUnionFind,
  UnionFind
  ) where

import           Perlude

import           Control.Lens               (view)
import           Control.Monad.ST           (ST)
import           Data.Array.MArray          (modifyArray, newGenArray,
                                             newListArray, readArray,
                                             writeArray)
import           Data.Generics.Labels       ()

import           Control.Monad              (when)
import           Data.Array.Base            (UArray, freeze, (!))
import           Internal.UnionFindInternal (MutableUnionFind (..))

newtype UnionFind = UnionFind { unUnionFind :: UArray Int Int }

new :: Int -> ST s (MutableUnionFind s)
new size =
  MutableUnionFind
  <$> newGenArray (0, size - 1) pure
  <*> newListArray (0, size - 1) (repeat 0)

union :: MutableUnionFind s -> Int -> Int -> ST s ()
union uf x y =
  do
    rootY <- find uf y
    rootX <- find uf x
    if rootY == rootX
      then return ()
      else merge uf rootX rootY

-- Merge x and y nodes, minimising the resulting rank
merge :: MutableUnionFind s -> Int -> Int -> ST s ()
merge uf x y =
  let
    roots = view #roots uf
    ranks = view #ranks uf
  in do
    rankY <- readArray ranks y
    rankX <- readArray ranks x
    if rankY > rankX
      then merge uf y x
      else
      do
        writeArray roots y x
        when (rankX == rankY) $  modifyArray ranks x (+1)

find :: MutableUnionFind s -> Int -> ST s Int
find uf x =
  let
    roots = view #roots uf
  in do
    parent <- readArray roots x
    if parent == x
      then return parent
      else
      do
        root <- find uf parent
        writeArray roots x root
        return root

toText :: MutableUnionFind s -> ST s Text
toText uf =
  do
    roots <- toArray <$> toUnionFind uf
    ranks :: UArray Int Int <- freeze $ view #ranks uf
    pure $  "Roots: " <> show roots <> "\nRanks: " <> show ranks

toUnionFind :: MutableUnionFind s -> ST s UnionFind
toUnionFind = fmap UnionFind . freeze . view #roots

toArray :: UnionFind -> UArray Int Int
toArray = unUnionFind

find' :: UnionFind -> Int -> Int
find' uf x =
  let
    array = unUnionFind uf
    parent = array ! x
  in
    if parent == x then x else find' uf parent
