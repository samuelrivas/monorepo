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
import           Control.Monad              (when)
import           Control.Monad.Identity     (Identity (..))
import           Control.Monad.ST           (ST)
import           Data.Array.Base            (freeze, (!))
import           Data.Array.IArray          (Array)
import           Data.Array.MArray          (modifyArray, newGenArray,
                                             newListArray, readArray,
                                             writeArray)
import           Data.Generics.Labels       ()
import           Data.Ix                    (Ix)
import           Internal.UnionFindInternal (MutableUnionFind (..))

class Monad m => UnionM u e m where
  union'' :: u -> e -> e -> m ()

instance Ix e => UnionM (MutableUnionFind s e) e (ST s) where
  union'' = union

class Monad m => FindM u e m where
  find'' :: u -> e -> m e

class Find u e where
  find''' :: u -> e -> e

instance Ix e => FindM (MutableUnionFind s e) e (ST s) where
  find'' = find

instance Ix e => FindM (UnionFind e) e Identity where
  find'' uf x = Identity $ find' uf x

instance Ix e => Find (UnionFind e) e where
  find''' = find'

newtype UnionFind a = UnionFind { unUnionFind :: Array a a }

new :: Ix a => a -> a -> ST s (MutableUnionFind s a)
new from to =
  MutableUnionFind
  <$> newGenArray (from, to) pure
  <*> newListArray (from, to) (repeat 0)

union :: Ix a => Eq a => MutableUnionFind s a -> a -> a -> ST s ()
union uf x y =
  do
    rootY <- find uf y
    rootX <- find uf x
    if rootY == rootX
      then return ()
      else merge uf rootX rootY

-- -- Merge x and y nodes, minimising the resulting rank
merge :: Ix a => MutableUnionFind s a -> a -> a -> ST s ()
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

find :: Ix a => MutableUnionFind s a -> a -> ST s a
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

toText :: Ix a => Show a => MutableUnionFind s a -> ST s Text
toText uf =
  do
    roots <- toArray <$> toUnionFind uf
    ranks :: Array a Int <- freeze $ view #ranks uf
    pure $  "Roots: " <> show roots <> "\nRanks: " <> show ranks

toUnionFind :: Ix a => MutableUnionFind s a -> ST s (UnionFind a)
toUnionFind = fmap UnionFind . freeze . view #roots

toArray :: UnionFind a -> Array a a
toArray = unUnionFind

find' :: Ix a => UnionFind a -> a -> a
find' uf x =
  let
    array = unUnionFind uf
    parent = array ! x
  in
    if parent == x then x else find' uf parent
