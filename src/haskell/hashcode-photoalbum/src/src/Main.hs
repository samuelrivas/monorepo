import qualified Data.Set       as Set
import qualified Data.Text.Lazy as T

{-# ANN module "HLint: ignore Use camelCase" #-}

data Orientation = H | V deriving (Eq, Ord, Show)

data Picture = Picture {
  id          :: Int,
  tags        :: Set.Set T.Text,
  orientation :: Orientation
  } deriving (Eq, Ord, Show)

type Slide = [Picture]
type Tags = Set.Set T.Text

mk_tags :: [String] -> Set.Set T.Text
mk_tags = Set.fromList . map T.pack

example :: [Picture]
example = [
   Picture {
      Main.id = 1,
      tags = mk_tags ["cat", "beach", "sun"],
      orientation = H
      },
   Picture {
      Main.id = 2,
      tags = mk_tags ["selfie", "smile"],
      orientation = V
      },
  Picture {
      Main.id = 3,
      tags = mk_tags ["garden", "selfie"],
      orientation = V
      },
  Picture {
      Main.id = 4,
      tags = mk_tags ["nature", "vacation"],
      orientation = V
      },
  Picture {
      Main.id = 5,
      tags = mk_tags ["moutain", "bird"],
      orientation = V
      },
  Picture {
      Main.id = 6,
      tags = mk_tags ["garden", "cat"],
      orientation = H
      },
  Picture {
      Main.id = 7,
      tags = mk_tags ["mountain", "nature"],
      orientation = H
      },
  Picture {
      Main.id = 8,
      tags = mk_tags ["mountain", "nature", "bird"],
      orientation = H
      }
  ]

get_tags :: Slide -> Set.Set T.Text
get_tags = foldl (flip $ Set.union . tags) Set.empty

interest_factor :: Tags -> Tags -> Int
interest_factor t1 t2 =
  let subsets =
        ($ t2) . ($ t1)
        <$> [Set.intersection, Set.difference, flip Set.difference]
  in minimum $ Set.size <$> subsets

find_next :: Foldable t => Slide -> t Picture -> Picture
find_next slide = let
  origin_tags = get_tags slide
  f acc@(best_interest, _) candidate_picture =
    let candidate_interest =
          interest_factor origin_tags (tags candidate_picture)

    in if candidate_interest > best_interest
       then (candidate_interest, candidate_picture)
       else acc
  in snd . foldl f (0, head slide)

main :: IO ()
main =  putStrLn "Hello World"
