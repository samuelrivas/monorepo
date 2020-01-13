-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Advent.Day7 where

import           Perlude

import           Control.Lens          (assign, use, uses, view, _1, _2, _3)
import           Control.Monad.Loops   (whileM)
import           Control.Monad.State   (MonadState, evalState)
import           Data.Functor.Identity (runIdentity)
import           Data.List             (permutations)
import           Data.List.NonEmpty    (NonEmpty (..), fromList, toList)
import           Data.Text             (pack, splitOn, unpack)
import           Data.Text.IO          (putStrLn)

import           Control.Monad.Intcode

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
example_1 :: [Integer]
example_1 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]

example_2 :: [Integer]
example_2 = [3,23,3,24,1002,24,10,24,1002,23,-1,23,
             101,5,23,23,1,24,23,23,4,23,99,0,0]

example_2_1 :: [Integer]
example_2_1 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
               27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

amplifier :: Monad m => [Integer] -> IntcodeT m Integer
amplifier inputs = do
  pushInput inputs
  runProgram
  getOutput >>= \case
    [out] -> do
      flushOutput
      pure out
    other ->
      error $ "got more than one output: " <> show other

run_amplifier :: [Integer] -> Integer -> Integer -> Integer
run_amplifier code input phase =
  view _1 . runIdentity $ eval (amplifier [phase, input]) code

run_amplifier_chain :: [Integer] -> Integer -> NonEmpty Integer -> Integer
run_amplifier_chain code input (phase :| []) = run_amplifier code input phase
run_amplifier_chain code input (phase :| x : t) =
  let stage_output = run_amplifier code input phase
  in run_amplifier_chain code stage_output (x :| t)

perms :: NonEmpty a -> [NonEmpty a]
perms = fmap fromList <$> permutations . toList

find_solution_1 :: [Integer] -> Integer
find_solution_1 code =
  maximum (run_amplifier_chain code 0 <$> perms (0 :| [1..4]))

data FeedbackChain = FeedbackChain
 { stages :: [IntcodeState],
   output :: Integer, -- Last stage output, 0 when not run
   status :: Status
  } deriving stock (Generic, Show)

init_amplifier :: [Integer] -> Integer -> IntcodeState
init_amplifier code phase =
  view _2 . runIdentity $ run (pushInput [phase]) code

feedback_chain :: [Integer] -> [Integer] -> FeedbackChain
feedback_chain code phases =
  FeedbackChain
  { stages = init_amplifier code <$> phases,
    output = 0,
    status = Running
  }

step_amplifier :: Integer -> IntcodeState -> (IntcodeState, Integer)
step_amplifier input state =
  let
    (out, state', _) =
      runIdentity $ runEmpty (reset state >> amplifier [input])
  in
    (state', out)

run_feedback_chain :: Integer -> [IntcodeState] -> [(IntcodeState, Integer)]
run_feedback_chain _input [] = []
run_feedback_chain input (state : t) =
  let (state', out) = step_amplifier input state
  in (state', out) : run_feedback_chain out t

step_loop :: MonadState FeedbackChain m => m Integer
step_loop = do
  chain <- use #stages
  out <- use #output
  let (states, outs) = unzip . run_feedback_chain out $ chain
  assign #stages states
  assign #output $ last outs
  assign #status $ view #status $ last states
  use #output

power_with_feedback_chain :: MonadState FeedbackChain m => m Integer
power_with_feedback_chain =
  let running = uses #status (`elem` [Running, Interrupted])
  in last <$> whileM running step_loop

get_power :: [Integer] -> [Integer] -> Integer
get_power code phases =
  let fc = feedback_chain code phases
  in evalState power_with_feedback_chain fc

find_solution_2 :: [Integer] -> Integer
find_solution_2 code = maximum (get_power code <$> permutations [5..9])

main :: IO ()
main = do
  code :: [Integer] <- codeForDay "7"

  putStrLn $  "Solution 1: " <> (show $ find_solution_1 code)
  putStrLn $  "Solution 2: " <> (show $ find_solution_2 code)
