{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

import qualified Data.Set       as Set
import qualified Data.Text.Lazy as T

{-# ANN module "HLint: ignore Use camelCase" #-}

data Picture_info = Picture_info {
  id   :: Int,
  tags :: Set.Set T.Text
  } deriving (Eq, Ord, Show)

data V = V deriving (Eq, Ord, Show)
data H = H deriving (Eq, Ord, Show)

data Picture a where
  Picture_v :: Picture_info -> Picture V
  Picture_h :: Picture_info -> Picture H
deriving instance Show a => Show (Picture a)
deriving instance Ord a => Ord (Picture a)
deriving instance Eq a => Eq (Picture a)

data Slide a where
  Slide_v :: Picture V -> Picture V -> Slide V
  Slide_h :: Picture H -> Slide H
deriving instance Show a => Show (Slide a)
deriving instance Ord a => Ord (Slide a)
deriving instance Eq a => Eq (Slide a)

mk_tags :: [String] -> Set.Set T.Text
mk_tags = Set.fromList . map T.pack

example :: Set.Set (Either (Picture H) (Picture V))
example = Set.fromList [
  Left $ Picture_h Picture_info {
      Main.id = 1,
      tags = mk_tags ["cat", "beach", "sun"]
      },
  Right $ Picture_v Picture_info {
      Main.id = 2,
      tags = mk_tags ["selfie", "smile"]
      },
  Right $ Picture_v Picture_info {
      Main.id = 3,
      tags = mk_tags ["garden", "selfie"]
      },
  Left $ Picture_h Picture_info {
      Main.id = 4,
      tags = mk_tags ["garden", "cat"]
      }
  ]

main :: IO ()
main =  putStrLn "Hello World"
