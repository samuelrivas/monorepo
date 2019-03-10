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
      Main.id = 4,
      tags = mk_tags ["moutain", "bird"],
      orientation = V
      },
  Picture {
      Main.id = 4,
      tags = mk_tags ["garden", "cat"],
      orientation = H
      },
  Picture {
      Main.id = 4,
      tags = mk_tags ["mountain", "nature"],
      orientation = H
      },
  Picture {
      Main.id = 4,
      tags = mk_tags ["mountain", "nature", "bird"],
      orientation = H
      }
  ]

get_tags :: Slide -> Set.Set T.Text
get_tags = foldl (flip $ Set.union . tags) Set.empty

interest_factor :: Slide -> Slide -> Int
interest_factor s1 s2 = let
  t1 = get_tags s1
  t2 = get_tags s2
  subsets = ($ t2) . ($ t1) <$> [Set.intersection,
                                 Set.difference,
                                 flip Set.difference]
  in
  minimum $ Set.size <$> subsets

-- find_best :: Foldable t => Slide -> t Picture_h -> Picture
-- find_best s ps =
--   let f acc@(best_score, _) p =
--         let candidate_slide = Slide_h p
--             score = interest_factor s candidate_slide
--         in
--         if score > best_score
--         then (score, candidate_slide)
--         else acc
--       best_slide = snd $ foldl f (0, s) ps
--   in
--     H $ get_picture best_slide

main :: IO ()
main =  putStrLn "Hello World"
