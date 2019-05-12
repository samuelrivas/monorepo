--{-# OPTIONS -Wno-unused-top-binds #-}
{-# LANGUAGE FlexibleContexts #-}

import           AnnealSlideshow
import           Control.Monad.Writer
import           Data.Random          (RVar)
import qualified Data.Random          as Random
import           Data.Set             (Set)
import qualified Data.Set             as Set
import qualified Data.Text.Lazy       as T
import qualified Data.Text.Lazy.IO    as TIO
import           Data.Vector          (Vector)
import qualified Data.Vector          as V
import           EagerSlideshow
import           Metrics
import           Parser
import           Picture
import           Slideshow
{-# ANN module "HLint: ignore Use camelCase" #-}

example :: Set Picture
example = Set.fromList [
   Picture {
      pos = 1,
      sort_key = 1,
      tag_list = mk_tags ["cat", "beach", "sun"],
      orientation = H
      },
   Picture {
      pos = 2,
      sort_key = 2,
      tag_list = mk_tags ["selfie", "smile"],
      orientation = V
      },
  Picture {
      pos = 3,
      sort_key = 3,
      tag_list = mk_tags ["garden", "selfie"],
      orientation = V
      },
  Picture {
      pos = 4,
      sort_key = 4,
      tag_list = mk_tags ["nature", "vacation"],
      orientation = V
      },
  Picture {
      pos = 5,
      sort_key = 5,
      tag_list = mk_tags ["mountain", "bird", "nature"],
      orientation = V
      },
  Picture {
      pos = 6,
      sort_key = 6,
      tag_list = mk_tags ["garden", "cat", "vacation", "sun"],
      orientation = H
      },
  Picture {
      pos = 7,
      sort_key = 7,
      tag_list = mk_tags ["mountain", "nature", "sun", "selfie"],
      orientation = H
      },
  Picture {
      pos = 8,
      sort_key = 8,
      tag_list = mk_tags ["mountain", "nature", "bird", "sun"],
      orientation = H
      },
  Picture {
      pos = 9,
      sort_key = 9,
      tag_list = mk_tags ["mountain", "nature", "river", "moon"],
      orientation = H
      },
  Picture {
      pos = 10,
      sort_key = 10,
      tag_list = mk_tags ["moon", "selfie", "smile"],
      orientation = V
      }
  ]

example_2 :: Set Picture
example_2 = Set.fromList [
   Picture {
      pos = 1,
      sort_key = 1,
      tag_list = mk_tags ["cat", "beach", "sun", "ocean"],
      orientation = H
      },
   Picture {
      pos = 2,
      sort_key = 2,
      tag_list = mk_tags ["selfie", "smile", "ship"],
      orientation = V
      },
  Picture {
      pos = 3,
      sort_key = 3,
      tag_list = mk_tags ["foo", "bar", "baz"],
      orientation = H
      }
  ]

example_text :: T.Text
example_text = T.pack
               "4\n\
               \H 3 cat beach sun\n\
               \V 2 selfie smile\n\
               \V 2 garden selfie\n\
               \H 2 garden cat"

show_slideshow :: Vector Slide -> T.Text
show_slideshow slideshow =
  let zipped = V.zip slideshow $ V.tail slideshow
      with_interest (s1, s2) = T.concat [T.pack "\n",
                                         T.pack . show $
                                         interest_factor (get_tags s1) (get_tags s2),
                                         T.pack "\n",
                                         show_slide s2]
      first_frame = show_slide $ V.head slideshow
      frames_with_interest = with_interest <$> zipped
      v_to_text = T.concat . V.toList
  in
    v_to_text $ V.cons first_frame frames_with_interest

read_lines :: IO [T.Text]
read_lines = T.lines <$> TIO.getContents

-- Just generate on the whole Int range
sort_gen :: RVar Int
sort_gen = Random.uniform minBound maxBound

line_to_picture :: Int -> T.Text -> IO Picture
line_to_picture pos' text =
  do
    sort_key' <- Random.sample sort_gen
    return $ parse_picture pos' sort_key' text

parse_lines :: [T.Text] -> WriterT Metrics IO (Set Picture)
parse_lines input_lines =
  let
    ids = iterate (+ 1) 0
  in do
    rpictures <- liftIO . traverse (uncurry line_to_picture) . zip ids . tail $
                 input_lines
    increment_counter_n "parsed pictures" (fromIntegral $ length rpictures)
    return $ Set.fromList rpictures

main_with_metrics :: WriterT Metrics IO ()
main_with_metrics = do
  input <- liftIO read_lines
  pictures <- parse_lines input
  slideshow <- make_slideshow pictures
  liftIO . putStrLn . T.unpack . show_slideshow $ slideshow
  liftIO . putStrLn $ "Total interest: " ++ (show . total_interest $ slideshow)

main :: IO ()
main =
  do
    metrics <- execWriterT main_with_metrics
    putStrLn $ "Metrics: " ++ show metrics

-- main :: IO ()
-- main =
--   let
--     (slideshow, metrics) = runWriter $ make_slideshow example_2
--   in do
--     putStrLn . T.unpack . show_slideshow $ slideshow
--     putStrLn $ "Total interest: " ++ (show . total_interest $ slideshow)
--     putStrLn $ "Metrics: " ++ show metrics
