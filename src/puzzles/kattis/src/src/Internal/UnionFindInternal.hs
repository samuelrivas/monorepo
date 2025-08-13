-- | Basic data structure to implement the Union Find algorithm with unboxed
-- types and sublinear complexity for bot union and find operations. We provide
-- a mutable implementation for ST, and an immutable implementation that doesn't
-- support union operations, at least for now. Further generalisations and
-- operations may be developed later.
--
-- See https://en.wikipedia.org/wiki/Disjoint-set_data_structure
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Internal.UnionFindInternal (
  MutableUnionFind (MutableUnionFind)
  ) where

import           Perlude

import           Data.Array.ST (STUArray)
import           GHC.Generics  (Generic)

data MutableUnionFind s = MutableUnionFind {
  -- | Each node is represented by an index in this array. The value at the
  -- index is the parent of the node. nodes that are their own parents are root
  -- nodes
  roots :: STUArray s Int Int,
  -- | A node rank is its maximum height. Nodes without children have rank 0,
  -- nodes with 1 child have at least rank 1. Rank has somewhat elusive
  -- properties, such as that a tree with rank n has, at least, 2^n nodes. And
  -- we get the maximum number of nodes at a give rank n with all subtrees of
  -- rank n have exactly 2^n nodes. While keeping the tree size also works, and
  -- is more precise, rank works correctly and requires less updates.
  ranks :: STUArray s Int Int
  } deriving stock (Generic)
