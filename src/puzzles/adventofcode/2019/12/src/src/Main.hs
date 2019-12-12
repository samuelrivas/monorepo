{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

import           Prelude               hiding (Left, Right, concat, getLine,
                                        putStrLn, readFile, show)

import           Control.Lens          (assign, at, modifying, non, over, set,
                                        toListOf, traverse, use, view, _1, _2,
                                        _3)
import           Control.Monad.Loops   (whileM_)
import           Control.Monad.State   (StateT, lift, runStateT)
import           Data.Foldable         (maximum, minimum)
import           Data.Functor.Identity (runIdentity)
import           Data.Generics.Labels  ()
import           Data.Map.Strict       (Map, empty, keys, size)
import           Data.Text             (Text, concat, intercalate, splitOn,
                                        unpack)
import           Data.Text.IO          (putStrLn, readFile)
import           GHC.Generics          (Generic)
import Data.Foldable (foldl')
import Control.Monad.State (State, get, modify, put)

import Internal

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

example_1 :: [Moon]
example_1 = mk_moon <$> [(-1, 0, 2), (2, -10, -7), (4, -8, 8), (3, 5, -1)]

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
  over #pos (`sum_coord` v)
  $ over #velocity (`sum_coord` v) moon

type ProblemMonad = State [Moon]

step :: ProblemMonad ()
step = do
  moons <- get
  let delta_vs = velocity_delta moons <$> moons
  modify $ zipWith update_moon delta_vs

main :: IO ()
main = undefined

