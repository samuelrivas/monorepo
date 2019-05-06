{-# OPTIONS -Wno-unused-top-binds #-}
{-# LANGUAGE FlexibleContexts #-}

import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Data.Random               (RVar)
import qualified Data.Random               as Random
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import qualified Data.Text.Lazy            as T
import qualified Data.Text.Lazy.IO         as TIO
import           Data.Vector               (Vector)
import qualified Data.Vector               as V
import           Metrics
import           Parser
import           Picture

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

interest_factor :: Tags -> Tags -> Int
interest_factor t1 t2 =
  let common = Set.size $ Set.intersection t1 t2
  in minimum [common, Set.size t1 - common, Set.size t2 - common]

total_interest :: Vector Slide -> Int
total_interest deck =
  let
    tags = get_tags <$> deck
  in
  sum $ uncurry interest_factor <$> V.zip tags (V.tail tags)

-- Limit recursion to avoid quadratic times
max_depth :: Int
max_depth = 400

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
  in snd . foldl f (-1, Nothing) . Set.filter is_v . Set.take max_depth

get_next_picture :: Tags -> Set Picture -> Maybe (Picture, Set Picture)
get_next_picture tags pictures =
  do
    next_picture <- find_next tags pictures
    return (next_picture, (`Set.delete` pictures) next_picture)

get_next_slide :: (MonadWriter Metrics m) => Tags -> Set Picture ->
                  MaybeT m (Slide, Set Picture)
get_next_slide tags pictures =
  do
    (next_picture, new_pictures) <- MaybeT . return $ get_next_picture tags pictures
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

make_slideshow :: (MonadWriter Metrics m) => Set Picture -> m (Vector Slide)
make_slideshow pictures = make_slideshow_rec (mk_tags []) pictures V.empty

make_slideshow_rec :: (MonadWriter Metrics m) =>
  Tags -> Set Picture -> Vector Slide -> m (Vector Slide)
make_slideshow_rec latest_tags pictures slideshow =
  do
    maybe_next_slide <- runMaybeT $ get_next_slide latest_tags pictures
    case maybe_next_slide of
      Nothing -> return $ V.reverse slideshow
      Just (next_slide, next_pictures) ->
        do
          increment_counter "rec step"
          make_slideshow_rec
            (get_tags next_slide) next_pictures (V.cons next_slide slideshow)

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
    rpictures <- liftIO . traverse (uncurry line_to_picture) . zip ids . tail $ input_lines
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
