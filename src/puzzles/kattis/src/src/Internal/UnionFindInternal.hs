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
  elems :: STUArray s Int Int,
  ranks :: STUArray s Int Int
  } deriving stock (Generic)
