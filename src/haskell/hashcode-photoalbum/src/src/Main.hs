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

example :: Set.Set Picture
example = Set.fromList [
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

main :: IO ()
main =  putStrLn "Hello World"
