{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Prelude              hiding (getLine)

import           Control.Lens         (assign, modifying, use, uses)
import           Control.Monad.Loops  (whileM_)
import           Control.Monad.State
import           Data.Array           (elems, (!), (//))
import           Data.Generics.Labels ()
import           Data.List            (find)
import           Data.Text            (splitOn, unpack)
import           Data.Text.IO         (getLine)

import           Internal.State

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
get_output = uses #memory (head . elems)

set_input :: Monad m => Int -> Int -> ProgramT m ()
set_input noun verb = modifying #memory (// [(1, noun), (2, verb)])

all_tests :: [(Int, Int)]
all_tests = (,) <$> [0..99] <*> [0..99]

test :: Monad m => Int -> Int -> ProgramT m Bool
test noun verb = do
  set_input noun verb
  run_program
  (== 19690720) <$> get_output

eval_test :: Monad m => ComputerState -> (Int, Int) -> m Bool
eval_test s (noun, verb) = evalStateT (test noun verb) s

main :: IO ()
main = do
  memory :: [Int] <- fmap (read . unpack) . splitOn "," <$> getLine

  result <- evalStateT (set_input 12 2 >> run_program >> get_output)
            (initial_state memory)
  putStrLn $  "Solution 1: " <> show result

  results <- sequence $ eval_test (initial_state memory) <$> all_tests
  let sol = find fst $ zip results all_tests
  case sol of
    Just (_, (noun, verb)) ->
      putStrLn $  "Solution 2: " <> show (noun * 100 + verb)
    Nothing ->
      putStrLn "Couldn't find solution 2"
