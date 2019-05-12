module Slideshow
  (
    interest_factor,
    total_interest
  ) where

import qualified Data.Set    as Set
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Picture

{-# ANN module "HLint: ignore Use camelCase" #-}

interest_factor :: Tags -> Tags -> Int
interest_factor t1 t2 =
  let common = Set.size $ Set.intersection t1 t2
  in minimum [common, Set.size t1 - common, Set.size t2 - common]

total_interest :: Vector Slide -> Int
total_interest deck =
  let
    tags = get_tags <$> deck
  in
  sum $ uncurry interest_factor <$> V.zip tags (V.tail tags)

