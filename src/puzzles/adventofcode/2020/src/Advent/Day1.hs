{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day1 where

import           Perlude

import           Data.List        (find, tails)
import           Data.Maybe       (fromJust)
import           Data.Text        (splitOn)

import           Advent.Templib   (Day (..), MonadAdvent, input, runAdventT,
                                   solutions)
import qualified System.IO.Advent as IOAdvent

targetValue :: Int
targetValue = 2020

example :: [Int]
example = [1721, 979, 366, 299, 675, 1456]

getInput :: MonadIO m => m Text
getInput =  IOAdvent.getInput D1

-- TODO: Remove the `init` hack
parse :: Text -> [Int]
parse = fmap read . init . splitOn "\n"

-- TODO: Generalise to traversable
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = return []
combinations n xs = do
  y:xs' <- tails xs
  ys <- combinations (n-1) xs'
  return (y:ys)

-- TODO: Prettyfy with a lens fold
solve1 :: [Int] -> Maybe Int
solve1 = fmap product <$> find ((== targetValue) . sum) . combinations 2

-- TODO: Prettyfy with a lens fold
solve2 :: [Int] -> Maybe Int
solve2 = fmap product <$> find ((== targetValue) . sum) . combinations 3

solve1' :: MonadAdvent [Int] m => m Int
solve1' = do
  p <- input
  return $ product . fromJust <$> find ((== targetValue) . sum) . combinations 2 $ p

solve2' :: MonadAdvent [Int] m => m Int
solve2' = do
  p <- input
  return $ product . fromJust <$> find ((== targetValue) . sum) . combinations 3 $ p

main :: IO ()
main = getInput >>= runAdventT (solutions solve1' solve2') . parse

