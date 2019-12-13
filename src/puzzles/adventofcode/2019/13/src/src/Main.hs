-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

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

import           Control.Lens          (view, assign, at, modifying, non, over, set,
                                        toListOf, traverse, use, view, _1, _2,
                                        _3)
import           Control.Monad.Loops   (whileM_)
import           Control.Monad.State   (StateT, lift, runStateT)
import           Data.Foldable         (maximum, minimum)
import           Data.Functor.Identity (runIdentity)
import           Data.Generics.Labels  ()
import           Data.Map.Strict       (Map, empty, keys, size, fromList)
import           Data.Text             (Text, concat, intercalate, splitOn,
                                        unpack)
import           Data.Text.IO          (putStrLn, readFile)
import           GHC.Generics          (Generic)

import           Intcode               hiding (initial_state)
import           Internal              hiding (initial_state)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

type Coord = (Int, Int)

data Tile = Empty | Wall | Block | Paddle | Ball
  deriving stock (Show, Eq, Enum)

encode :: (Num c, Enum a) => a -> c
encode = fromIntegral . fromEnum

decode :: (Integral c, Enum a) => c -> a
decode = toEnum . fromIntegral

read_game :: [Integer] -> [Integer]
read_game code =
  view _1 . runIdentity $ launch (run_program >> get_output) code

parse_game :: [Integer] -> [(Coord, Tile)]
parse_game (x : y : 2 : t) =
 ((fromIntegral x, fromIntegral y), Block) : parse_game t
parse_game (x : y : _ : t) = parse_game t -- we ignore other tiles for now
parse_game [] = []
parse_game _ = undefined

get_input :: IO [Integer]
get_input = fmap (read . unpack) . splitOn "," <$> readFile "input.txt"

main :: IO ()
main = do
  code <- get_input
  putStrLn $ "Solution 1: " <> show (size . fromList . parse_game . read_game $ code)
