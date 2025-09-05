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

import           Control.Monad              (when)
import           Control.Monad.Identity     (Identity (..))
import           Control.Monad.ST           (ST)
import           Data.Array.Base            (MArray (..), freeze, safeIndex,
                                             safeRangeSize, unsafeNewArray_,
                                             (!))
import           Data.Array.IArray          (Array)
import           Data.Array.MArray          (Ix (range), newListArray,
                                             readArray, writeArray)
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
  do
    rankY <- readArray (ranks uf) y
    rankX <- readArray (ranks uf) x
    if rankY > rankX
      then merge uf y x
      else
      do
        writeArray (roots uf) y x
        when (rankX == rankY) $  modifyArray (ranks uf) x (+1)


find :: Ix a => MutableUnionFind s a -> a -> ST s a
find uf x =
  do
    parent <- readArray (roots uf) x
    if parent == x
      then return parent
      else
      do
        root <- find uf parent
        writeArray (roots uf) x root
        return root

toText :: Ix a => Show a => MutableUnionFind s a -> ST s Text
toText uf =
  do
    roots <- toArray <$> toUnionFind uf
    ranks :: Array a Int <- freeze $ ranks uf
    pure $  "Roots: " <> show roots <> "\nRanks: " <> show ranks

toUnionFind :: Ix a => MutableUnionFind s a -> ST s (UnionFind a)
toUnionFind = fmap UnionFind . freeze . roots

toArray :: UnionFind a -> Array a a
toArray = unUnionFind

find' :: Ix a => UnionFind a -> a -> a
find' uf x =
  let
    array = unUnionFind uf
    parent = array ! x
  in
    if parent == x then x else find' uf parent


-- Copied from Array 0.5.8.0, as kattis has an older version
-- =========================================================
{-# INLINE modifyArray #-}
-- | Modify an element in a mutable array
--
-- @since 0.5.6.0
modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray marr i f = do
  (l,u) <- getBounds marr
  n <- getNumElements marr
  let idx = safeIndex (l,u) n i
  x <- unsafeRead marr idx
  unsafeWrite marr idx (f x)

{-# INLINE newGenArray #-}
-- | Constructs a mutable array using a generator function.
-- It invokes the generator function in ascending order of the indices.
--
-- @since 0.5.6.0
newGenArray :: (MArray a e m, Ix i) => (i,i) -> (i -> m e) -> m (a i e)
newGenArray bnds f = do
    let n = safeRangeSize bnds
    marr <- unsafeNewArray_ bnds
    let g ix k i
            | i == n    = return ()
            | otherwise = do
                x <- f ix
                unsafeWrite marr i x
                k (i+1)
    foldr g (\ !_i -> return ()) (range bnds) 0
    -- The bang above is important for GHC for unbox the Int.
    return marr
