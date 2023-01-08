-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Advent.Day12 where

import           Control.Lens          (Getter, _1, _2, _3, each, over, set,
                                        sumOf, to, toListOf, view)
import           Control.Monad         (replicateM_)
import           Control.Monad.Loops   (whileM)
import           Control.Monad.State   (State, evalState, execState, get,
                                        modify)
import           Data.Foldable         (foldl')
import           Data.Generics.Labels  ()

import           Advent.Day12.Internal

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

example_1 :: [Moon]
example_1 = mk_moon <$> [(-1, 0, 2), (2, -10, -7), (4, -8, 8), (3, 5, -1)]

example_2 :: [Moon]
example_2 = mk_moon <$> [(-8, -10, 0), (5, 5, 10), (2, -7, 3), (9, -8, -3)]

input :: [Moon]
input = mk_moon <$> [(-10, -10, -13), (5, 5, -9), (3, 8, -16), (1, 3, -3)]

sum_coord :: Coord -> Coord -> Coord
sum_coord (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

to_delta :: Integer -> [Integer] -> Integer
to_delta pullee pullers =
  let
    f delta n =
      case compare pullee n of
        LT -> delta + 1
        GT -> delta - 1
        EQ -> delta
  in
    foldl' f 0 pullers

velocity_delta :: [Moon] -> Moon -> Coord
velocity_delta moons moon =
  let
    (xs, ys, zs) = unzip3 . toListOf (traverse . #pos) $ moons
    (x, y, z) = view #pos moon
  in
    (to_delta x xs, to_delta y ys, to_delta z zs)

update_moon :: Coord -> Moon -> Moon
update_moon v moon =
  let new_v = sum_coord (view #velocity moon) v
  in
    set #velocity new_v
    $ over #pos (sum_coord new_v) moon

type ProblemMonad = State [Moon]

step :: ProblemMonad ()
step = do
  moons <- get
  let delta_vs = velocity_delta moons <$> moons
  modify $ zipWith update_moon delta_vs

read_dimension :: Getter Coord Integer -> ProblemMonad ([Integer], [Integer])
read_dimension getter = do
  moons <- get
  pure (toListOf (traverse . #pos . getter) moons,
        toListOf (traverse . #velocity . getter) moons)

run_steps :: Int -> [Moon] -> [Moon]
run_steps n  = execState (replicateM_ n step)

energy :: Moon -> Integer
energy m = sumOf (#pos . each . to abs) m
           * sumOf (#velocity . each . to abs) m

solution_1 :: Integer
solution_1 =
  let moons = run_steps 1000 input
  in sum $ energy <$> moons

-- Find the cycle sequence for a given dimension (_1, _2, or _3). Return the
-- list of positions and velocities for each step for debugging purposes
find_cycle :: [Moon] -> Getter Coord Integer -> [([Integer], [Integer])]
find_cycle moons getter =
  flip evalState moons $ do
  initial <- read_dimension getter
  step
  no_cycle <- whileM ((/= initial) <$> read_dimension getter) $ do
    moons' <-  read_dimension getter
    step
    pure moons'
  pure (initial : no_cycle)

solution_2 :: Int
solution_2 =
  let
    xs = length $ find_cycle input _1
    ys = length $ find_cycle input _2
    zs = length $ find_cycle input _3
  in
    lcm xs $ lcm ys zs

main :: IO ()
main = do
  putStrLn $ "Solution 1: " <> show solution_1
  putStrLn $ "Solution 2: " <> show solution_2
