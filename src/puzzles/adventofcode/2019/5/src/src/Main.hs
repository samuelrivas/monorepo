{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Prelude              hiding (getLine)

import           Control.Lens         (assign, modifying, over, use, uses, view,
                                       _2)
import           Control.Monad.Loops  (whileM_)
import           Control.Monad.State  (MonadState, StateT, evalStateT,
                                       execStateT, put, runStateT)
import           Data.Array           (elems, listArray, (!), (//))
import           Data.Generics.Labels ()
import           Data.List            (find)
import           Data.Text            (splitOn, unpack)
import           Data.Text.IO         (getLine)
import           GHC.Generics         (Generic)

import           Internal

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
type ProgramT m a = StateT ComputerState m a

initial_state :: [Int] -> ComputerState
initial_state list = ComputerState Running 0
  $ listArray (0, length list - 1) list

read_immediate :: MonadState ComputerState m => Int -> m Int
read_immediate pos = (! pos) <$> use #memory

read_position :: MonadState ComputerState m => Int -> m Int
read_position pos = read_immediate pos >>= read_immediate

-- Return the Opcode and the amount of parameters it takes
parse_opcode :: Int -> Maybe (Opcode, Int)
parse_opcode 1  = Just (Add, 3)
parse_opcode 2  = Just (Mul, 3)
parse_opcode 99 = Just (Halt, 0)
parse_opcode _  = Nothing

parse_mode :: Int -> Maybe Mode
parse_mode 0 = Just Position
parse_mode 1 = Just Value
parse_mode _ = Nothing

factor :: Int -> [Int]
factor 0 = []
factor x = factor (x `div` 10) ++ [x `mod` 10]

parse_instruction :: Int -> Maybe Instruction
parse_instruction value =
  let
    int_opcode = value `mod` 100
    int_modes = reverse . factor $ value `div` 10
  in do
    (opcode, parameters) <- parse_opcode int_opcode
    modes <- sequence $ parse_mode <$> int_modes
    pure $ Instruction opcode modes

next_opcode :: Monad m => ProgramT m (Maybe Opcode)
next_opcode = do
  pp <- use #pp
  fmap fst . parse_opcode <$> read_immediate pp

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

load_program :: Monad m => [Int] -> ProgramT m ()
load_program = put . initial_state

run_program :: Monad m => ProgramT m ()
run_program = whileM_ ((== Running) <$> use #status) step_program

get_output :: Monad m => ProgramT m Int
get_output = uses #memory (! 0)

dump_memory :: Monad m => ProgramT m [Int]
dump_memory = uses #memory elems

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

launch :: ProgramT m a -> [Int] -> m (a, ComputerState)
launch program memory = runStateT program (initial_state memory)

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
