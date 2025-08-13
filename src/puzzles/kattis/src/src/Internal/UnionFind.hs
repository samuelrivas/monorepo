-- TODO
-- Generalise types to MArray and Array if reasonable
-- Convert into opaque monad
-- Extract tests
-- Union by rank to prevent unbalanced trees
-- Add a print tree for debugging
{-# LANGUAGE NoImplicitPrelude #-}

module Internal.UnionFind where

import           Perlude

import           Control.Monad     (void)
import           Control.Monad.ST  (ST, runST)
import           Data.Array.MArray (Ix, MArray, freeze, modifyArray,
                                    newGenArray, readArray)
import           Data.Array.ST     (STUArray)

new :: Int -> ST s (STUArray s Int Int)
new size = newGenArray (0, size - 1) pure

union :: STUArray s Int Int -> Int -> Int -> ST s ()
union array x y =
  do
    repY <- find array y
    modifyArray array x (const repY)

find :: STUArray s Int Int -> Int -> ST s Int
find array x =
  do
    rep <- readArray array x
    if rep == x
      then return rep
      else
      do
        rep' <- find array rep
        modifyArray array x (const rep')
        return rep'

-- Tests, to move elsewhere

-- test1 =
--   runST $ do
--   uf <- new 10
--   freeze uf
