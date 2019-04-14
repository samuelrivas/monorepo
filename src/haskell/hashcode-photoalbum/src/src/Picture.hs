module Picture
  ( Picture (..)
  , Slide
  , Orientation (..)
  , Tags
  , mk_tags
  , mk_orientation
  , get_tags
  , show_tags
  , show_slide
  ) where

import qualified Data.Set       as Set
import qualified Data.Text.Lazy as T

{-# ANN module "HLint: ignore Use camelCase" #-}

data Orientation = H | V deriving (Eq, Ord, Show)

type Tags = Set.Set T.Text

data Picture = Picture {
  pos         :: Int, -- ^ Position in the original file, 0 indexed
  sort_key    :: Int, -- ^ Random key for shuffling in the set
  tag_list    :: Tags,
  orientation :: Orientation
  } deriving (Eq, Show)

instance Ord Picture where
  compare x y = compare (sort_key x) (sort_key y)

type Slide = [Picture]

mk_tags :: [String] -> Tags
mk_tags = Set.fromList . map T.pack

mk_orientation :: T.Text -> Orientation
mk_orientation t
  | t == v = V
  | t == h = H
  | otherwise = error "Invalid orientation"
  where
    v = T.pack "V"
    h = T.pack "H"

get_tags :: Slide -> Tags
get_tags = foldl (flip $ Set.union . tag_list) Set.empty

show_tags :: Slide -> T.Text
show_tags = T.unwords . Set.toAscList . get_tags

show_slide :: Slide -> T.Text
show_slide slide =
  let
    poss = T.pack . show . pos <$> slide
    format = T.pack . show . orientation <$> slide
    show_part (i, p) = T.concat [i, p]
  in
    T.concat [T.unwords $ show_part <$> zip poss format,
               T.pack " | ", show_tags slide]
