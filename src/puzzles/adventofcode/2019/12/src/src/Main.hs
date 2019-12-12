{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import           Control.Lens          (assign, at, each, modifying, non, over,
                                        set, sumOf, to, toListOf, traverse, use,
                                        view, _1, _2, _3)
import           Control.Monad         (replicateM_)
import           Control.Monad.Loops   (whileM_)
import           Control.Monad.State   (State, execState, get, modify, put)
import           Data.Foldable         (foldl')
import           Data.Functor.Identity (runIdentity)
import           Data.Generics.Labels  ()
import           Data.Map.Strict       (Map, empty, keys, size)
import           Data.Text             (Text, concat, intercalate, splitOn,
                                        unpack)
import           GHC.Generics          (Generic)

import           Internal

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

example_1 :: [Moon]
example_1 = mk_moon <$> [(-1, 0, 2), (2, -10, -7), (4, -8, 8), (3, 5, -1)]

example_2 :: [Moon]
example_2 = mk_moon <$> [(-8, -10, 0), (5, 5, 10), (2, -7, 3), (9, -8, -3)]

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

run_steps :: Int -> [Moon] -> [Moon]
run_steps n  = execState (replicateM_ n step)

energy :: Moon -> Integer
energy m = sumOf (#pos . each . to abs) m
           * sumOf (#velocity . each . to abs) m

input :: [Moon]
input = mk_moon <$> [(-10, -10, -13),
                     (5, 5, -9),
                     (3, 8, -16),
                     (1, 3, -3)]

solution_1 :: Integer
solution_1 =
  let moons = run_steps 1000 input
  in sum $ energy <$> moons

main :: IO ()
main =
  putStrLn $ "Solution 1: " <> show solution_1
