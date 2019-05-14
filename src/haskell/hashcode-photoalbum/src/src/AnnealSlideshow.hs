{-# OPTIONS -Wno-unused-top-binds #-}
{-# LANGUAGE FlexibleContexts #-}
module AnnealSlideshow
  (
    anneal_slideshow
  )
where

import qualified Annealing
import           Control.Monad.Writer
import           Data.Maybe
import           Data.Random          (RVar)
import qualified Data.Random          as Random
import qualified Data.Set             as Set
import           Data.Vector          (Vector, (!), (!?), (//))
import qualified Data.Vector          as V
import           Metrics
import           Picture
import           Slideshow

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Since there are 24 possible permuatations, but each permuatation has 4 copies
-- that are essentially the same (different flips of the same two pictures in
-- each slide), we will generate a no op with 1/6 probability.
blend_verticals :: Vector Slide ->  Int -> Int -> RVar (Vector Slide)
blend_verticals slideshow pos_1 pos_2 =
  let
    slide_1 = slideshow ! pos_1
    slide_2 = slideshow ! pos_2
  in do
    [s1, s2, s3, s4] <- Random.shuffle (slide_1 ++ slide_2)
    return $ slideshow // [(pos_1, [s1, s2]), (pos_2, [s3, s4])]

swap_pictures :: Vector Slide ->  Int -> Int -> RVar (Vector Slide)
swap_pictures slideshow pos_1 pos_2 =
  let
    slide_1 = slideshow ! pos_1
    slide_2 = slideshow ! pos_2
  in
    if get_orientation slide_1 == V && get_orientation slide_2 == V
    then blend_verticals slideshow pos_1 pos_2
    else return $ slideshow // [(pos_1, slide_2), (pos_2, slide_1)]

-- There is a special condition when two vertical slides are next to each other,
-- we cannot just count the interest between them twice as it could yield
-- incorrect results when they are not swapped as if they were full slides
-- (i.e. when we blend them). Thus, we dedup the edges. This would not be needed
-- if all slides were indivisible, like horizontal slides
neighbourhood_interest :: Vector Slide -> Int -> Int -> Int
neighbourhood_interest slideshow center_1 center_2 =
  let
    edges_of slide = Set.fromList [(slide - 1, slide), (slide, slide + 1)]
    edges = Set.toList $ Set.union (edges_of center_1) (edges_of center_2)
  in
    sum $ edge_interest slideshow <$> edges

-- Returns 0 if the edge doesn't exist
edge_interest :: Vector Slide -> (Int, Int) -> Int
edge_interest slideshow (pos_1, pos_2) =
  let
    tags_1 = get_tags <$> slideshow !? pos_1
    tags_2 = get_tags <$> slideshow !? pos_2
  in
    fromMaybe 0 $ interest_factor <$> tags_1 <*> tags_2

mutate_slideshow :: Vector Slide -> Int -> Int -> RVar (Int, Vector Slide)
mutate_slideshow slideshow pos_1 pos_2 =
  let
    interest_out = neighbourhood_interest slideshow pos_1 pos_2
  in do
    mutant <- swap_pictures slideshow pos_1 pos_2
    let interest_in = neighbourhood_interest mutant pos_1 pos_2
    return (interest_in - interest_out, mutant)

slideshow_gen :: (Double, Vector Slide) -> RVar (Double, Vector Slide)
slideshow_gen (cost, slideshow) =
  let len = V.length slideshow
      position_gen = Random.Uniform 0 (len - 1)
  in do
    position_1 <- Random.sample position_gen
    position_2 <- Random.sample position_gen
    if position_1 == position_2
      then return (cost, slideshow)
      else do
      (diff, mutant) <- mutate_slideshow slideshow position_1 position_2
      return (cost - fromIntegral diff, mutant)

anneal_slideshow ::
  Vector Slide -> WriterT Metrics RVar (Int, Vector Slide)
anneal_slideshow slideshow =
  let score = total_interest slideshow
      config = (Annealing.default_config $ Annealing.MkGen slideshow_gen) {
        Annealing.initial_temp = 15,
        Annealing.steps_per_temp = 1000,
        Annealing.cooldown_ratio = 0.97
        }
      starting_point = (-fromIntegral score, slideshow)
      from_cost (a, x) = (round (-a), x)
      anneal_m = Annealing.run_anneal (Annealing.anneal_to_iteration 100)
  in from_cost <$> anneal_m config starting_point
