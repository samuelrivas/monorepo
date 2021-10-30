-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Advent.Day7 where

import           Prelude               hiding (getLine, putStrLn, readFile)

import           Advent.Day7.Intcode
import           Advent.Day7.Internal
import           Control.Lens          (_2, _3, assign, use, uses, view)
import           Control.Monad.Loops   (whileM)
import           Control.Monad.State   (MonadState, evalState)
import           Data.Advent           (Day (..))
import           Data.Functor.Identity (runIdentity)
import           Data.Generics.Labels  ()
import           Data.List             (permutations)
import           Data.List.NonEmpty    (NonEmpty (..), fromList, toList)
import           Data.Text             (pack, splitOn, unpack)
import           Data.Text.IO          (putStrLn)
import           GHC.Generics          (Generic)
import           System.IO.Advent      (getInput)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
example_1 :: [Int]
example_1 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]

example_2 :: [Int]
example_2 = [3,23,3,24,1002,24,10,24,1002,23,-1,23,
             101,5,23,23,1,24,23,23,4,23,99,0,0]

example_2_1 :: [Int]
example_2_1 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
               27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

run_amplifier :: [Int] -> Int -> Int -> Int
run_amplifier code input phase =
  let out = view _3 . runIdentity $
        launch (push_input [phase, input] >> run_program) code
  in read . unpack $ out

run_amplifier_chain :: [Int] -> Int -> NonEmpty Int -> Int
run_amplifier_chain code input (phase :| []) = run_amplifier code input phase
run_amplifier_chain code input (phase :| x : t) =
  let stage_output = run_amplifier code input phase
  in run_amplifier_chain code stage_output (x :| t)

perms :: NonEmpty a -> [NonEmpty a]
perms = fmap fromList <$> permutations . toList

find_solution_1 :: [Int] -> Int
find_solution_1 code =
  maximum (run_amplifier_chain code 0 <$> perms (0 :| [1..4]))

data FeedbackChain = FeedbackChain
 { stages :: [ComputerState],
   output :: Int, -- Last stage output, 0 when not run
   status :: Status
  } deriving stock (Generic, Show)

init_amplifier :: [Int] -> Int -> ComputerState
init_amplifier code phase =
  view _2 . runIdentity $ run (push_input [phase]) (initial_state code)

feedback_chain :: [Int] -> [Int] -> FeedbackChain
feedback_chain code phases =
  FeedbackChain
  { stages = init_amplifier code <$> phases,
    output = 0,
    status = Running
  }

step_amplifier :: Int -> ComputerState -> (ComputerState, Int)
step_amplifier input state =
  let
    ((), state', out) =
      runIdentity $ run (push_input [input] >> run_program) state
  in
    (state', read . unpack $ out)

run_feedback_chain :: Int -> [ComputerState] -> [(ComputerState, Int)]
run_feedback_chain _input [] = []
run_feedback_chain input (state : t) =
  let (state', out) = step_amplifier input state
  in (state', out) : run_feedback_chain out t

step_loop :: MonadState FeedbackChain m => m Int
step_loop = do
  chain <- use #stages
  out <- use #output
  let (states, outs) = unzip . run_feedback_chain out $ chain
  assign #stages states
  assign #output $ last outs
  assign #status $ view #status $ last states
  use #output

power_with_feedback_chain :: MonadState FeedbackChain m => m Int
power_with_feedback_chain =
  let running = uses #status (`elem` [Running, Interrupted])
  in last <$> whileM running step_loop

get_power :: [Int] -> [Int] -> Int
get_power code phases =
  let fc = feedback_chain code phases
  in evalState power_with_feedback_chain fc

find_solution_2 :: [Int] -> Int
find_solution_2 code = maximum (get_power code <$> permutations [5..9])

main :: IO ()
main = do
  code :: [Int] <- fmap (read . unpack) . splitOn "," <$> getInput D7

  putStrLn $  "Solution 1: " <> (pack . show $ find_solution_1 code)
  putStrLn $  "Solution 2: " <> (pack . show $ find_solution_2 code)
