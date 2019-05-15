module Parser
  ( parse_picture
  ) where

import qualified Data.Set       as Set
import qualified Data.Text.Lazy as T
import           Picture

{-# ANN module "HLint: ignore Use camelCase" #-}

parse_picture :: Int -> Int -> T.Text -> Picture
parse_picture pos' sort_key' t =
  let
    (orient_str:_:tags) = T.words t
  in
    Picture {
    pos = pos',
    sort_key = sort_key',
    orientation = mk_orientation orient_str,
    tag_list = Set.fromList tags
    }
