-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Advent.Day8 where

import           Prelude

import           Data.List        (find, minimumBy, transpose)
import           Data.Maybe       (fromMaybe)
import           Data.Text        (unpack)

import           System.IO.Advent (getInput)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

split_layers :: Int -> String -> [String]
split_layers _digits [] = []
split_layers digits image =
  take digits image : split_layers digits (drop digits image)

count :: Char -> String -> Int
count c = length . filter (== c)

fewest :: Char -> [String] -> String
fewest c = minimumBy (\a b -> compare (count c a) (count c b))

solution_1 :: String -> Int
solution_1 img =
  let layer = fewest '0' $ split_layers (25 * 6) img
  in count '1' layer * count '2' layer

fuse_layers :: [String] -> String
fuse_layers layers =
  let fuse_bytes = fromMaybe '2' . find (/= '2')
  in fuse_bytes <$> transpose layers

render_pixel :: Char -> Char
render_pixel '1' = 'X'
render_pixel '0' = ' '
render_pixel _   = '.'

render_layer :: Int -> String -> [String]
render_layer width = split_layers width . fmap render_pixel

solution_2 :: String -> [String]
solution_2 img =
  let layers = split_layers (25 * 6) img
  in render_layer 25 . fuse_layers $ layers

main :: IO ()
main = do
  image <- unpack <$> getInput "8"
  putStrLn $ "Solution 1: " <> show (solution_1 image)
  putStrLn "Solution 2:"
  sequence_ $ putStrLn  <$> solution_2 image
