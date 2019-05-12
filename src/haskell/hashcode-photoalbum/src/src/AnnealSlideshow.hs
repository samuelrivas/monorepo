{-# OPTIONS -Wno-unused-top-binds #-}
module AnnealSlideshow
  (
    anneal_slideshow
  )
where

import qualified Annealing
import           Data.Random (RVar)
import qualified Data.Random as Random
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Metrics
import           Picture
import           Slideshow

{-# ANN module "HLint: ignore Use camelCase" #-}

swap_pictures :: Vector Slide ->  Int -> Int -> Vector Slide
swap_pictures slideshow _ _ = slideshow

mutate_slideshow :: (Int, Vector Slide) -> RVar (Int, Vector Slide)
mutate_slideshow (interest, slideshow) =
  let len = V.length slideshow
      position_gen = Random.Uniform 0 (len - 1)
  in do
    position_1 <- Random.sample position_gen
    position_2 <- Random.sample position_gen
    return (interest, swap_pictures slideshow position_1 position_2)

slideshow_gen :: (Double, Vector Slide) -> RVar (Double, Vector Slide)
slideshow_gen = return

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
