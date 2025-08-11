-- TODO
-- Generalise types to MArray and Array if reasonable
-- Convert into opaque monad
-- Extract tests
{-# LANGUAGE NoImplicitPrelude #-}

module Internal.UnionFind where

import           Perlude

import           Control.Monad.ST  (ST, runST)
import           Data.Array.MArray (Ix, MArray, freeze, modifyArray,
                                    newGenArray, readArray)
import           Data.Array.ST     (STUArray)

new :: Int -> ST s (STUArray s Int Int)
new size = newGenArray (0, size - 1) pure

union :: STUArray s Int Int -> Int -> Int -> ST s ()
union array x y =
  modifyArray array x (const y)

find :: STUArray s Int Int -> Int -> ST s Int
find = readArray


-- Tests, to move elsewhere

-- test1 =
--   runST $ do
--   uf <- new 10
--   freeze uf
