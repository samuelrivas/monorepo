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

module Advent.Day16 where

import           Prelude          hiding (lines, putStrLn, readFile, show,
                                   unlines)
import qualified Prelude

import           Data.Advent      (Day (..))
import           Data.List        (concatMap, unfoldr)
import           Data.Text        (Text, pack, unpack)
import qualified Data.Text        as Text
import           Data.Text.IO     (putStrLn)
import           System.IO.Advent (getInput)

show :: Show a => a -> Text
show = pack . Prelude.show

-- Pretty sure this won't work for efficiency reasons, but gonna try anyway.
phaseSignal :: Int -> [Int]
phaseSignal pos = drop 1 . cycle $ concatMap (replicate pos) [0, 1, 0, -1]

convol :: [Int] -> [Int] -> Int
convol x y  = sum $ zipWith (*) x y

step :: Int -> [Int] -> Int
step n  = (`mod` 10) . abs . convol (phaseSignal n)

phase :: [Int] -> [Int]
phase l = take (length l) $ unfoldr (\b -> Just (step b l, b + 1)) 1

solve1 :: [Int] -> Text
solve1 input = Text.concat . fmap show . take 8 $ foldl (.) id (replicate 100 phase) input

solve2 :: [Int] -> Int
solve2 = undefined

main :: IO ()
main = do
  input <- fmap ((+ (- fromEnum '0')) . fromEnum) . unpack <$> getInput D16
  putStrLn $ "Solution 1: " <> solve1 input
  -- putStrLn $ "Solution 2: " <> show (solve2 input)
  putStrLn "Solution 2: Takes forever :("
