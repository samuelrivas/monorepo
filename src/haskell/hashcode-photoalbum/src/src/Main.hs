import qualified Data.Maybe     as Maybe
import qualified Data.Set       as Set
import qualified Data.Text.Lazy as T

{-# ANN module "HLint: ignore Use camelCase" #-}

data Orientation = H | V deriving (Eq, Ord, Show)

data Picture = Picture {
  id          :: Int,
  tag_list    :: Set.Set T.Text,
  orientation :: Orientation
  } deriving (Eq, Ord, Show)

type Slide = [Picture]
type Tags = Set.Set T.Text

mk_tags :: [String] -> Set.Set T.Text
mk_tags = Set.fromList . map T.pack

example :: Set.Set Picture
example = Set.fromList [
   Picture {
      Main.id = 1,
      tag_list = mk_tags ["cat", "beach", "sun"],
      orientation = H
      },
   Picture {
      Main.id = 2,
      tag_list = mk_tags ["selfie", "smile"],
      orientation = V
      },
  Picture {
      Main.id = 3,
      tag_list = mk_tags ["garden", "selfie"],
      orientation = V
      },
  Picture {
      Main.id = 4,
      tag_list = mk_tags ["nature", "vacation"],
      orientation = V
      },
  Picture {
      Main.id = 5,
      tag_list = mk_tags ["moutain", "bird", "nature"],
      orientation = V
      },
  Picture {
      Main.id = 6,
      tag_list = mk_tags ["garden", "cat", "vacation", "sun"],
      orientation = H
      },
  Picture {
      Main.id = 7,
      tag_list = mk_tags ["mountain", "nature", "sun", "selfie"],
      orientation = H
      },
  Picture {
      Main.id = 8,
      tag_list = mk_tags ["mountain", "nature", "bird", "sun"],
      orientation = H
      }
  ]

get_tags :: Slide -> Set.Set T.Text
get_tags = foldl (flip $ Set.union . tag_list) Set.empty

interest_factor :: Tags -> Tags -> Int
interest_factor t1 t2 =
  let subsets =
        ($ t2) . ($ t1)
        <$> [Set.intersection, Set.difference, flip Set.difference]
  in minimum $ Set.size <$> subsets

find_next :: Foldable t => Tags -> t Picture -> Maybe Picture
find_next origin_tags = let
  f acc@(best_interest, _) candidate_picture =
    let candidate_interest =
          interest_factor origin_tags (tag_list candidate_picture)

    in if candidate_interest > best_interest
       then (candidate_interest, Just candidate_picture)
       else acc
  in snd . foldl f (-1, Nothing)

get_next :: Tags -> Set.Set Picture -> (Maybe Picture, Set.Set Picture)
get_next tags pictures =
  let
    next_picture = find_next tags pictures
    new_pictures = maybe pictures (`Set.delete` pictures) next_picture
  in
    (next_picture, new_pictures)

make_slideshow :: Set.Set Picture -> [[Picture]]
make_slideshow pictures =
  make_slideshow_rec (mk_tags []) pictures []

make_slideshow_rec :: Tags -> Set.Set Picture -> [[Picture]] -> [[Picture]]
make_slideshow_rec latest_tags pictures slideshow =
  if Set.null pictures
  then reverse slideshow
  else
    let (maybe_next_picture, next_pictures) = get_next latest_tags pictures
        next_picture = Maybe.fromJust maybe_next_picture
        next_tags = tag_list next_picture
    in
      make_slideshow_rec next_tags next_pictures ([next_picture]:slideshow)

main :: IO ()
main =  putStrLn "Hello World"
