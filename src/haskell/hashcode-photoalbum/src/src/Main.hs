{-# OPTIONS -Wno-unused-top-binds #-}
{-# LANGUAGE FlexibleContexts #-}

import           Control.Monad.Writer
import           Data.Set             (Set)
import qualified Data.Set             as Set
import qualified Data.Text.Lazy       as T
import qualified Data.Text.Lazy.IO    as TIO
import           Metrics
import           Parser
import           Picture

{-# ANN module "HLint: ignore Use camelCase" #-}

example :: Set Picture
example = Set.fromList [
   Picture {
      ident = 1,
      tag_list = mk_tags ["cat", "beach", "sun"],
      orientation = H
      },
   Picture {
      ident = 2,
      tag_list = mk_tags ["selfie", "smile"],
      orientation = V
      },
  Picture {
      ident = 3,
      tag_list = mk_tags ["garden", "selfie"],
      orientation = V
      },
  Picture {
      ident = 4,
      tag_list = mk_tags ["nature", "vacation"],
      orientation = V
      },
  Picture {
      ident = 5,
      tag_list = mk_tags ["mountain", "bird", "nature"],
      orientation = V
      },
  Picture {
      ident = 6,
      tag_list = mk_tags ["garden", "cat", "vacation", "sun"],
      orientation = H
      },
  Picture {
      ident = 7,
      tag_list = mk_tags ["mountain", "nature", "sun", "selfie"],
      orientation = H
      },
  Picture {
      ident = 8,
      tag_list = mk_tags ["mountain", "nature", "bird", "sun"],
      orientation = H
      },
  Picture {
      ident = 9,
      tag_list = mk_tags ["mountain", "nature", "river", "moon"],
      orientation = H
      },
  Picture {
      ident = 10,
      tag_list = mk_tags ["moon", "selfie", "smile"],
      orientation = V
      }
  ]

example_2 :: Set Picture
example_2 = Set.fromList [
   Picture {
      ident = 1,
      tag_list = mk_tags ["cat", "beach", "sun", "ocean"],
      orientation = H
      },
   Picture {
      ident = 2,
      tag_list = mk_tags ["selfie", "smile", "ship"],
      orientation = V
      },
  Picture {
      ident = 3,
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

interest_factor :: Tags -> Tags -> Int
interest_factor t1 t2 =
  let common = Set.size $ Set.intersection t1 t2
  in minimum [common, Set.size t1 - common, Set.size t2 - common]

total_interest :: [Slide] -> Int
total_interest deck =
  let
    tags = get_tags <$> deck
  in
  sum $ uncurry interest_factor <$> zip tags (tail tags)

-- Limit recursion to avoid quadratic times
max_depth :: Int
max_depth = 200

find_next :: Tags -> Set Picture -> Maybe Picture
find_next origin_tags = let
  f acc@(best_interest, _) candidate_picture =
    let candidate_interest =
          interest_factor origin_tags (tag_list candidate_picture)

    in if candidate_interest > best_interest
       then (candidate_interest, Just candidate_picture)
       else acc
  in snd . foldl f (-1, Nothing) . Set.take max_depth

find_next_v :: Tags -> Picture -> Set Picture -> Maybe Picture
find_next_v origin_tags v_picture =
  let f acc@(best_interest, _) candidate_picture =
        let candidate_interest =
              interest_factor origin_tags (get_tags [v_picture,
                                                     candidate_picture])
        in if candidate_interest > best_interest
           then (candidate_interest, Just candidate_picture)
           else acc
      is_v = (V ==) . orientation
  in snd . foldl f (-1, Nothing) . Set.take max_depth . Set.filter is_v

get_next_picture :: Tags -> Set Picture -> Maybe (Picture, Set Picture)
get_next_picture tags pictures =
  do
    next_picture <- find_next tags pictures
    return (next_picture, (`Set.delete` pictures) next_picture)

get_next_slide :: Tags -> Set Picture ->
  WriterT Metrics Maybe (Slide, Set Picture)
get_next_slide tags pictures =
  do
    (next_picture, new_pictures) <- lift $ get_next_picture tags pictures
    if V == orientation next_picture
      then
      do
        increment_counter "partial V slide"

        case find_next_v tags next_picture new_pictures of
          Just next_v ->
            do
              increment_counter "completed V slide"
              return ([next_picture, next_v], Set.delete next_v new_pictures)

          Nothing ->
            do
              increment_counter "failed V slide"
              get_next_slide tags new_pictures
      else
      do
        increment_counter "H slide"
        return ([next_picture], new_pictures)

make_slideshow :: (MonadWriter Metrics m) => Set Picture -> m [Slide]
make_slideshow pictures = make_slideshow_rec (mk_tags []) pictures []

make_slideshow_rec :: (MonadWriter Metrics m) =>
  Tags -> Set Picture -> [Slide] -> m [Slide]
make_slideshow_rec latest_tags pictures slideshow =
  let
    maybe_next_slide = runWriterT $ get_next_slide latest_tags pictures
  in
    case maybe_next_slide of
      Nothing -> return $ reverse slideshow
      Just something ->
        do
          (next_slide, next_pictures) <- writer something
          increment_counter "rec step"
          make_slideshow_rec
            (get_tags next_slide) next_pictures (next_slide:slideshow)

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

read_lines :: IO [T.Text]
read_lines = T.lines <$> TIO.getContents

parse_lines :: (MonadWriter Metrics m) => [T.Text] -> m (Set Picture)
parse_lines input_lines =
  let
    ids = iterate (+ 1) 0
    pictures = Set.fromList . fmap (uncurry parse_picture) . zip ids . tail $ input_lines
  in do
    increment_counter_n "parsed pictures" (fromIntegral $ Set.size pictures)
    return pictures

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
--   do
--     input <- read_lines
--     let slideshow = make_slideshow $ parse_lines input
--       in do
--       putStrLn . T.unpack . show_slideshow $ slideshow
--       putStrLn $ "Total interest: " ++ (show . total_interest $ slideshow)

-- main :: IO ()
-- main =
--   let
--     slideshow = make_slideshow example
--   in do
--     putStrLn . T.unpack . show_slideshow $ slideshow
--     putStrLn $ "Total interest: " ++ (show . total_interest $ slideshow)
