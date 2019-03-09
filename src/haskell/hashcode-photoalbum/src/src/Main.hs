import qualified Data.Set       as Set
import qualified Data.Text.Lazy as T

{-# ANN module "HLint: ignore Use camelCase" #-}

data Picture_info = Picture_info {
  id   :: Int,
  tags :: Set.Set T.Text
  } deriving (Eq, Ord, Show)

newtype Picture_v = Picture_v Picture_info deriving (Eq, Ord, Show)
newtype Picture_h = Picture_h Picture_info deriving (Eq, Ord, Show)

data Picture = V Picture_v | H Picture_h deriving (Eq, Ord, Show)

data Slide = Slide_v Picture_v Picture_v
  | Slide_h Picture_h
  deriving (Eq, Ord, Show)

mk_v_picture :: Picture_info -> Picture
mk_v_picture = V . Picture_v

mk_h_picture :: Picture_info -> Picture
mk_h_picture = H . Picture_h

mk_tags :: [String] -> Set.Set T.Text
mk_tags = Set.fromList . map T.pack

get_info :: Picture -> Picture_info
get_info (V (Picture_v info)) = info
get_info (H (Picture_h info)) = info

example :: [Picture]
example = [
   mk_h_picture Picture_info {
      Main.id = 1,
      tags = mk_tags ["cat", "beach", "sun"]
      },
  mk_v_picture Picture_info {
      Main.id = 2,
      tags = mk_tags ["selfie", "smile"]
      },
  mk_v_picture Picture_info {
      Main.id = 3,
      tags = mk_tags ["garden", "selfie"]
      },
  mk_v_picture Picture_info {
      Main.id = 4,
      tags = mk_tags ["nature", "vacation"]
      },
  mk_v_picture Picture_info {
      Main.id = 4,
      tags = mk_tags ["moutain", "bird"]
      },
  mk_h_picture Picture_info {
      Main.id = 4,
      tags = mk_tags ["garden", "cat"]
      },
  mk_h_picture Picture_info {
      Main.id = 4,
      tags = mk_tags ["mountain", "nature"]
      },
  mk_h_picture Picture_info {
      Main.id = 4,
      tags = mk_tags ["mountain", "nature", "bird"]
      }
  ]

slide_example_1 :: Slide
slide_example_1 = Slide_h $ Picture_h
   Picture_info {
      Main.id = 1,
      tags = mk_tags ["cat", "dog", "beach", "sun"]
      }

slide_example_2 = Slide_v
  (Picture_v Picture_info {
    Main.id = 1,
    tags = mk_tags ["beach", "sun", "parrots"]
  })
  (Picture_v Picture_info {
  Main.id = 1,
  tags = mk_tags ["sun", "jiraffes"]
  })

get_tags :: Slide -> Set.Set T.Text
get_tags (Slide_v p1 p2) =
  let extract_tags (Picture_v info) = tags info
  in Set.union (extract_tags p1) (extract_tags p2)
get_tags (Slide_h (Picture_h p)) =
  tags p

interest_factor :: Slide -> Slide -> Int
interest_factor s1 s2 = let
  t1 = get_tags s1
  t2 = get_tags s2
  in
  minimum $ Set.size <$> [Set.intersection t1 t2,
                          Set.difference t1 t2,
                          Set.difference t2 t1]

main :: IO ()
main =  putStrLn "Hello World"
