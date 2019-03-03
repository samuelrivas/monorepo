import qualified Data.Set       as Set
import qualified Data.Text.Lazy as T

data Picture_info = Picture_info {
  id   :: Int,
  tags :: Set.Set T.Text
  } deriving (Eq, Ord)

data Picture = H Picture_info | V Picture_info
  deriving (Eq, Ord)

make_tags :: [String] -> Set.Set T.Text
make_tags = Set.fromList . map T.pack

example :: Set.Set Picture
example = Set.fromList [
  H Picture_info {
      Main.id = 1,
      tags = Set.fromList $ T.pack <$> ["cat", "beach", "sun"]
      },
  V Picture_info {
      Main.id = 2,
      tags = Set.fromList $ T.pack <$> ["selfie", "smile"]
      },
  V Picture_info {
      Main.id = 3,
      tags = make_tags ["garden", "selfie"]
      },
  H Picture_info {
      Main.id = 4,
      tags = make_tags ["garden", "cat"]
      }
  ]
main :: IO ()
main =  putStrLn "Hello World"
