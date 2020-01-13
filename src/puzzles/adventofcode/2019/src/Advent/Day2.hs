{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day2 (main) where

import           Perlude

import           Control.Lens          (modifying, uses, view, _1)
import qualified Data.HashMap.Strict   as Map
import           Data.List             (find)

import           Control.Monad.Intcode

-- Note that we should not use the Intcode machine actual read and write
-- operations here, these are required in future days, but are not supposed to
-- exist yet.

readOutput :: Monad m => IntcodeT m Integer
readOutput = uses #memory (Map.! 0)

writeInput :: Monad m => Integer -> Integer -> IntcodeT m ()
writeInput noun verb =
  modifying #memory (Map.union . Map.fromList $ [(1, noun), (2, verb)])

allTest :: [(Integer, Integer)]
allTest = (,) <$> [0..99] <*> [0..99]

test :: Monad m => Integer -> Integer -> IntcodeT m Bool
test noun verb = do
  writeInput noun verb
  runProgram
  (== 19690720) <$> readOutput

evalTest :: Monad m => [Integer] -> (Integer, Integer) -> m Bool
evalTest code (noun, verb) = view _1 <$> eval (test noun verb) code

main :: IO ()
main = do
  code <- codeForDay "2"

  (result, _) <- eval (writeInput 12 2 >> runProgram >> readOutput) code
  putStrLn $  "Solution 1: " <> show result

  results <- sequence $ evalTest code <$> allTest
  let sol = find fst $ zip results allTest
  case sol of
    Just (_, (noun, verb)) ->
      putStrLn $  "Solution 2: " <> show (noun * 100 + verb)
    Nothing ->
      putStrLn "Couldn't find solution 2"
