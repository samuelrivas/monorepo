-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Advent.Day9 where

import           Perlude

import           Control.Lens          (view, _1)
import           Data.Functor.Identity (runIdentity)

import           Control.Monad.Intcode

example1 :: [Integer]
example1 = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]

example2 :: [Integer]
example2 = [1102,34915192,34915192,7,4,7,99,0]

example3 :: [Integer]
example3 = [104,1125899906842624,99]

solution :: Integer -> [Integer] -> Integer
solution input =
  head . view _1 . runIdentity .
  eval (pushInput [input] >> runProgram >> getOutput)

solution1 :: [Integer] -> Integer
solution1 = solution 1

solution2 :: [Integer] -> Integer
solution2 = solution 2

main :: IO ()
main = do
  code :: [Integer] <- codeForDay "9"
  putStrLn $ "Solution 1: " <> show (solution1 code)
  putStrLn $ "Solution 2: " <> show (solution2 code)
