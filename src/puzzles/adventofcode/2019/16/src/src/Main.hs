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

import           Prelude              hiding (lines, readFile, unlines, show, putStrLn)
import qualified           Prelude

import           Control.Lens         (at, non, over, set, traverse, view, _1,
                                       _2)
import           Data.Foldable        (foldl')
import           Data.Generics.Labels ()
import           Data.Maybe           (fromMaybe)
import           Data.Ratio           ((%))
import           Data.Text            (Text, intercalate, lines, splitOn,
                                       unpack, pack)
import qualified Data.Text            as Text
import           Data.Text.IO         (readFile, putStrLn)
import Data.List (concatMap, unfoldr)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

show :: Show a => a -> Text
show = pack . Prelude.show

phaseSignal :: Int -> [Int]
phaseSignal pos = drop 1 . cycle $ concatMap (replicate pos) [0, 1, 0, -1]

convol :: [Int] -> [Int] -> Int
convol x y  = sum $ zipWith (*) x y

step :: Int -> [Int] -> Int
step n  = (`mod` 10) . abs . convol (phaseSignal n)

phase :: [Int] -> [Int]
phase l = take (length l) $ unfoldr (\b -> Just (step b l, b + 1)) 1

solve1 :: [Int] -> Int
solve1 = undefined

solve2 :: [Int] -> Int
solve2 = undefined

getInput :: IO [Int]
getInput = fmap (read . unpack) . splitOn "," <$> readFile "input.txt"

main :: IO ()
main = do
  input <- getInput
  putStrLn $ "Solution 1: " <> show (solve1 input)
  putStrLn $ "Solution 2: " <> show (solve2 input)
