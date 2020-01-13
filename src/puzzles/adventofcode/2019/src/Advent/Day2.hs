{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day2 where

import           Perlude

import           Control.Lens          (assign, modifying, use, uses, view, _1)
import           Control.Monad.Loops   (whileM_)
import           Control.Monad.State
import           Data.Array            ((!), (//))
import           Data.Generics.Labels  ()
import qualified Data.HashMap.Strict   as Map
import           Data.List             (find)
import qualified Data.Text             as Text

import           Advent.Day2.State
import qualified Control.Monad.Intcode as Intcode
import           System.IO.Advent      (getInput)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

data Opcode = Add | Mul | Halt
  deriving stock Show

type ProgramT m a = StateT ComputerState m a

parse_opcode :: Int -> Maybe Opcode
parse_opcode 1  = Just Add
parse_opcode 2  = Just Mul
parse_opcode 99 = Just Halt
parse_opcode _  = Nothing

next_opcode :: Monad m => ProgramT m (Maybe Opcode)
next_opcode = do
  pp <- use #pp
  memory <- use #memory
  pure $ parse_opcode (memory ! pp)

step_program :: Monad m => ProgramT m ()
step_program =
  next_opcode >>= \case
    Just opc -> run_opcode opc
    Nothing -> assign #status Aborted

run_opcode :: Monad m => Opcode -> ProgramT m ()
run_opcode Halt = assign #status Finished
run_opcode Add  = run_arith (+)
run_opcode Mul  = run_arith (*)

run_arith :: Monad m => (Int -> Int -> Int) -> ProgramT m ()
run_arith op = do
  pp <- use #pp
  memory <- use #memory
  let
    x = memory ! (memory ! (pp + 1))
    y = memory ! (memory ! (pp + 2))
    dest = memory ! (pp + 3)

  modifying #memory (// [(dest, op x y)])
  modifying #pp (+ 4)

run_program :: Monad m => ProgramT m ()
run_program = whileM_ ((== Running) <$> use #status) step_program

get_output :: Monad m => ProgramT m Int
get_output = uses #memory (! 0)

get_output' :: Monad m => Intcode.IntcodeT m Integer
get_output' = uses #memory (Map.! 0)

set_input :: Monad m => Int -> Int -> ProgramT m ()
set_input noun verb = modifying #memory (// [(1, noun), (2, verb)])

set_input' :: Monad m => Integer -> Integer -> Intcode.IntcodeT m ()
set_input' noun verb =
  modifying #memory (Map.union . Map.fromList $ [(1, noun), (2, verb)])

all_tests :: [(Int, Int)]
all_tests = (,) <$> [0..99] <*> [0..99]

all_tests' :: [(Integer, Integer)]
all_tests' = (,) <$> [0..99] <*> [0..99]

test :: Monad m => Int -> Int -> ProgramT m Bool
test noun verb = do
  set_input noun verb
  run_program
  (== 19690720) <$> get_output

test' :: Monad m => Integer -> Integer -> Intcode.IntcodeT m Bool
test' noun verb = do
  set_input' noun verb
  Intcode.runProgram
  (== 19690720) <$> get_output'

eval_test :: Monad m => ComputerState -> (Int, Int) -> m Bool
eval_test s (noun, verb) = evalStateT (test noun verb) s

eval_test' :: Monad m => [Integer] -> (Integer, Integer) -> m Bool
eval_test' code (noun, verb) = view _1 <$> Intcode.eval (test' noun verb) code

main :: IO ()
main = do
  memory :: [Int] <- fmap (read . unpack) . Text.splitOn "," <$> getInput "2"
  code <- Intcode.codeForDay "2"

  result <- evalStateT (set_input 12 2 >> run_program >> get_output)
            (initial_state memory)
  (result', _) <- Intcode.eval (set_input' 12 2 >> Intcode.runProgram >> get_output') code
  putStrLn $  "Solution 1: " <> show result
  putStrLn $  "Solution 1: " <> show result'

  results <- sequence $ eval_test (initial_state memory) <$> all_tests
  results' <- sequence $ eval_test' code <$> all_tests'
  let sol = find fst $ zip results all_tests
      sol' = find fst $ zip results' all_tests'
  case sol of
    Just (_, (noun, verb)) ->
      putStrLn $  "Solution 2: " <> show (noun * 100 + verb)
    Nothing ->
      putStrLn "Couldn't find solution 2"
  case sol' of
    Just (_, (noun, verb)) ->
      putStrLn $  "Solution 2: " <> show (noun * 100 + verb)
    Nothing ->
      putStrLn "Couldn't find solution 2"
