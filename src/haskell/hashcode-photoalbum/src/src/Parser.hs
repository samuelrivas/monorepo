module Parser
  ( parse_picture
  ) where

import qualified Data.Set       as Set
import qualified Data.Text.Lazy as T
import           Picture

{-# ANN module "HLint: ignore Use camelCase" #-}

parse_picture :: Int -> T.Text -> Picture
parse_picture picture_id t =
  let
    (orient_str:_:tags) = T.words t
  in
    Picture {
    ident = picture_id,
    orientation = mk_orientation orient_str,
    tag_list = Set.fromList tags
    }
