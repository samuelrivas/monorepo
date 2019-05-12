{-# OPTIONS -Wno-unused-top-binds #-}
module AnnealSlideshow
  -- (
  --   anneal_slideshow,
  --   swap_pictures
  -- )
where

import qualified Annealing
import           Data.Maybe
import           Data.Random (RVar)
import qualified Data.Random as Random
import           Data.Vector (Vector, (!), (!?), (//))
import qualified Data.Vector as V
import           Metrics
import           Picture
import           Slideshow

{-# ANN module "HLint: ignore Use camelCase" #-}

-- We will not care bout vertical slides initaily, we treat them as
-- indivisible. After we get this working, we can think of adding more mutations
-- for the case when the swapped slides are both vertical

swap_pictures :: Vector Slide ->  Int -> Int -> Vector Slide
swap_pictures slideshow pos_1 pos_2 =
  slideshow // [(pos_1, slideshow ! pos_2), (pos_2, slideshow ! pos_1)]


-- Calculate the interest of the slide in position center with its neighbours
neighbourhood_interest :: Vector Slide -> Int -> Int
neighbourhood_interest slideshow center =
  let
    neighbour_interest = edge_interest slideshow center
  in
    neighbour_interest (center - 1) + neighbour_interest (center + 1)

-- Returns 0 if the edge doesn't exist
edge_interest :: Vector Slide -> Int -> Int -> Int
edge_interest slideshow pos_1 pos_2 =
  let
    tags_1 = get_tags <$> slideshow !? pos_1
    tags_2 = get_tags <$> slideshow !? pos_2
  in
    fromMaybe 0 $ interest_factor <$> tags_1 <*> tags_2


mutate_slideshow :: Vector Slide -> Int -> Int -> (Int, Vector Slide)
mutate_slideshow slideshow pos_1 pos_2 =
  let
    interest_out = sum
      $ neighbourhood_interest slideshow <$> [pos_1, pos_2]
    mutant = swap_pictures slideshow pos_1 pos_2
    interest_in = sum
      $ neighbourhood_interest mutant <$> [pos_1, pos_2]
  in
    (interest_in - interest_out, mutant)

slideshow_gen :: (Double, Vector Slide) -> RVar (Double, Vector Slide)
slideshow_gen (cost, slideshow) =
  let len = V.length slideshow
      position_gen = Random.Uniform 0 (len - 1)
  in do
    position_1 <- Random.sample position_gen
    position_2 <- Random.sample position_gen
    let (diff, mutant) = mutate_slideshow slideshow position_1 position_2
    return (cost - fromIntegral diff, mutant)

anneal_slideshow :: Vector Slide -> RVar ((Int, Vector Slide), Metrics)
anneal_slideshow slideshow =
  let score = total_interest slideshow
      config = Annealing.default_config $ Annealing.MkGen slideshow_gen
  in do
      (anneal_state, metrics) <- Annealing.exec_anneal_t
                                 (Annealing.anneal_to_temp 0.1)
                                 config
                                 (fromIntegral (-score), slideshow)
      return ((-(round . Annealing.min_cost $ anneal_state),
               Annealing.best_sol anneal_state),
              metrics)
