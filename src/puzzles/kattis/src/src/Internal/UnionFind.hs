-- TODO
-- Generalise types to MArray and Array if reasonable
-- Generalise over ST and IO
-- Convert into opaque monad
-- Extract tests
-- Union by rank to prevent unbalanced trees
-- Add a print tree for debugging
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}

module Internal.UnionFind (
  find,
  new,
  toArray,
  union,

  MutableUnionFind
  ) where

import           Perlude

import           Control.Lens               (over, view)
import           Control.Monad.ST           (ST)
import           Data.Array.MArray          (modifyArray, newGenArray,
                                             newListArray, readArray)
import           Data.Array.ST              (STUArray)
import           Data.Generics.Labels       ()

import           Data.Array.Base            (UArray, freeze)
import           Internal.UnionFindInternal (MutableUnionFind (..))

new :: Int -> ST s (MutableUnionFind s)
new size =
  MutableUnionFind
  <$> newGenArray (0, size - 1) pure
  <*> newListArray (0, size - 1) (repeat 0)

union :: MutableUnionFind s -> Int -> Int -> ST s ()
union uf x y =
  let
    elems = view #elems uf
  in do
    repY <- find uf y
    modifyArray elems x (const repY)

find :: MutableUnionFind s -> Int -> ST s Int
find uf x =
  let
    elems = view #elems uf
  in do
    rep <- readArray elems x
    if rep == x
      then return rep
      else
      do
        rep' <- find uf rep
        modifyArray elems x (const rep')
        return rep'

-- FIXME: Proper interface to move into immutable values
toArray :: MutableUnionFind s -> ST s (UArray Int Int)
toArray = freeze . view #elems
