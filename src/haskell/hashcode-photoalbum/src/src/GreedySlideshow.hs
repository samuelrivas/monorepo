{-# LANGUAGE FlexibleContexts #-}

module GreedySlideshow
  (
    make_slideshow
  ) where

import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Vector               (Vector)
import qualified Data.Vector               as V
import           Metrics
import           Picture
import           Slideshow

{-# ANN module "HLint: ignore Use camelCase" #-}

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

-- | Given a set of picture, create a slideshow using an eager algorithm with
-- pruned lookahead (that is, the result is likely fairly good, but surely not
-- optimal)
--
-- Shorten lookahead by limiting max_depth for faster evaluation at the cost of
-- likely worse solutions
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
