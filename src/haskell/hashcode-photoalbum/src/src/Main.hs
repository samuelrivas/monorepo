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
      },
  Picture {
      Main.id = 9,
      tag_list = mk_tags ["mountain", "nature", "river", "moon"],
      orientation = H
      }
  ]

get_tags :: Slide -> Set.Set T.Text
get_tags = foldl (flip $ Set.union . tag_list) Set.empty

show_tags :: Slide -> T.Text
show_tags = T.unwords . Set.toAscList . get_tags

show_slide :: Slide -> T.Text
show_slide slide =
  let ids = T.pack . show . Main.id <$> slide
      format = T.pack . show . orientation <$> slide
      show_part (i, p) = T.concat [i, p]
  in T.concat [T.unwords $ show_part <$> zip ids format,
               T.pack " | ", show_tags slide]

interest_factor :: Tags -> Tags -> Int
interest_factor t1 t2 =
  let subsets =
        ($ t2) . ($ t1)
        <$> [Set.intersection, Set.difference, flip Set.difference]
  in minimum $ Set.size <$> subsets

total_interest :: [Slide] -> Int
total_interest deck =
  let
    tags = get_tags <$> deck
  in
  sum $ uncurry interest_factor <$> zip tags (tail tags)

find_next :: Foldable t => Tags -> t Picture -> Maybe Picture
find_next origin_tags = let
  f acc@(best_interest, _) candidate_picture =
    let candidate_interest =
          interest_factor origin_tags (tag_list candidate_picture)

    in if candidate_interest > best_interest
       then (candidate_interest, Just candidate_picture)
       else acc
  in snd . foldl f (-1, Nothing)

find_next_v :: Tags -> Picture -> Set.Set Picture -> Maybe Picture
find_next_v origin_tags v_picture =
  let f acc@(best_interest, _) candidate_picture =
        let candidate_interest =
              interest_factor origin_tags (get_tags [v_picture,
                                                     candidate_picture])
        in if candidate_interest > best_interest
           then (candidate_interest, Just candidate_picture)
           else acc
      is_v = (V ==) . orientation
  in snd . foldl f (-1, Nothing) . Set.filter is_v

get_next_picture :: Tags -> Set.Set Picture -> Maybe (Picture, Set.Set Picture)
get_next_picture tags pictures =
  do
    next_picture <- find_next tags pictures
    return (next_picture, (`Set.delete` pictures) next_picture)

get_next_slide :: Tags -> Set.Set Picture -> Maybe (Slide, Set.Set Picture)
get_next_slide tags pictures =
  do
    (next_picture, new_pictures) <- get_next_picture tags pictures
    if V == orientation next_picture
      then do
      next_v <- find_next_v tags next_picture new_pictures
      return ([next_picture, next_v], Set.delete next_v new_pictures)
      else return ([next_picture], new_pictures)

make_slideshow :: Set.Set Picture -> [Slide]
make_slideshow pictures =
  make_slideshow_rec (mk_tags []) pictures []

-- FIXME Remove that fromJust!
make_slideshow_rec :: Tags -> Set.Set Picture -> [Slide] -> [Slide]
make_slideshow_rec latest_tags pictures slideshow =
  if Set.null pictures
  then reverse slideshow
  else
    let (next_slide, next_pictures) =
          Maybe.fromJust $ get_next_slide latest_tags pictures
        next_tags = get_tags next_slide
    in
      make_slideshow_rec next_tags next_pictures (next_slide:slideshow)

show_slideshow :: [Slide] -> T.Text
show_slideshow slideshow =
  let zipped = zip slideshow $ tail slideshow
      with_interest (s1, s2) = T.concat [T.pack "\n",
                                         T.pack . show $
                                         interest_factor (get_tags s1) (get_tags s2),
                                         T.pack "\n",
                                         show_slide s2]
  in
    T.concat $ show_slide (head slideshow) : (with_interest <$> zipped)

main :: IO ()
main =
  let
    slideshow = make_slideshow example
  in do
    putStrLn . T.unpack . show_slideshow $ slideshow
    putStrLn $ "Total interest: " ++ (show . total_interest $ slideshow)
