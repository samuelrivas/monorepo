{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

import           Prelude               hiding (getLine)

import           Control.Lens          (assign, ix, modifying, over, preview,
                                        use, uses, view, _2)
import           Control.Monad         (MonadPlus, mzero)
import           Control.Monad.Fail    (MonadFail (..))
import           Control.Monad.Loops   (whileM_)
import           Control.Monad.Reader  (MonadReader)
import           Control.Monad.RWS.CPS (MonadRWS, RWST, evalRWST, execRWST, get,
                                        put, runRWST, tell)
import           Control.Monad.State   (MonadState, StateT, evalStateT,
                                        execStateT, put, runStateT)
import           Control.Monad.Writer  (MonadWriter)
import           Data.Array            (elems, listArray, (!), (//))
import           Data.Generics.Labels  ()
import           Data.List             (find)
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text, pack, splitOn, unpack)
import           Data.Text.IO          (getLine)
import           GHC.Generics          (Generic)

import           Internal

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
newtype ProgramT m a = ProgramT { unProgramT :: RWST () Text ComputerState m a }
  deriving newtype (Functor, Applicative, Monad, MonadWriter Text,
                    MonadState ComputerState, MonadReader ())


eval :: Monad m => ProgramT m a -> ComputerState -> m (a, Text)
eval = flip evalRWST () . unProgramT

run :: Monad m => ProgramT m a -> ComputerState -> m (a, ComputerState, Text)
run = flip runRWST () . unProgramT

exec :: Monad m => ProgramT m a -> ComputerState -> m (ComputerState, Text)
exec = flip execRWST () . unProgramT

-- Extend modes with infinite 0s so that we are safe reading them
get_mode :: Int -> [Mode] -> Mode
get_mode pos = fromMaybe Position . preview (ix pos)

get_3_modes :: [Mode] -> (Mode, Mode, Mode)
get_3_modes modes =
  let get_mode' = flip get_mode modes
  in (get_mode' 0, get_mode' 1, get_mode' 2)


parse_opcode :: Int -> [Mode] -> Maybe Opcode
parse_opcode 1 modes   = Just $ Add $ get_3_modes modes
parse_opcode 2 modes   = Just $ Mul $ get_3_modes modes
parse_opcode 99 _modes = Just Halt
parse_opcode _ _       = Nothing

parse_mode :: Int -> Maybe Mode
parse_mode 0 = Just Position
parse_mode 1 = Just Immediate
parse_mode _ = Nothing

factor :: Int -> [Int]
factor 0 = []
factor x = factor (x `div` 10) ++ [x `mod` 10]

parse_instruction_value :: Int -> Maybe Opcode
parse_instruction_value value =
  let
    int_opcode = value `mod` 100
    int_modes = reverse . factor $ value `div` 100
  in sequence (parse_mode <$> int_modes) >>= parse_opcode int_opcode

pop_opcode :: Monad m => ProgramT m (Maybe Opcode)
pop_opcode = parse_instruction_value <$> pop_value

initial_state :: [Int] -> ComputerState
initial_state list = ComputerState Running 0
  $ listArray (0, length list - 1) list

read_immediate :: Monad m => Int -> ProgramT m Int
read_immediate pos = (! pos) <$> use #memory

read_position :: Monad m => Int -> ProgramT m Int
read_position pos = read_immediate pos >>= read_immediate

step_program :: Monad m => ProgramT m ()
step_program =
  pop_opcode >>= \case
    Just opc -> run_opcode opc
    Nothing -> abort "could not pop next opcode"

abort :: Monad m => Text -> ProgramT m ()
abort msg = do
  tell $ "Something went wrong: " <> msg <> "\n"
  tell ">>>>>>>> Dump\n"
  get >>= tell . pack . show
  assign #status Aborted

run_opcode :: Monad m => Opcode -> ProgramT m ()
run_opcode Halt        = assign #status Finished
run_opcode (Add modes) = run_arith (+) modes
run_opcode (Mul modes) = run_arith (*) modes

-- Read the next value in memory, and advance program counter
pop_value :: Monad m => ProgramT m Int
pop_value = do
  value <- use #pp >>= read_immediate
  modifying #pp (+1)
  pure value

read_parameter :: Monad m => Mode -> ProgramT m Int
read_parameter Immediate = pop_value
read_parameter Position  = pop_value >>= read_immediate

run_arith ::
  Monad m =>
  (Int -> Int -> Int) ->
  (Mode, Mode, Mode) ->
  ProgramT m ()
run_arith op (mode_1, mode_2, Position) = do
  x <- read_parameter mode_1
  y <- read_parameter mode_2
  dest <- read_parameter Immediate

  modifying #memory (// [(dest, op x y)])
run_arith _ _ = assign #status Aborted

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
eval_test s (noun, verb) = fst <$> eval (test noun verb) s

launch :: Monad m => ProgramT m a -> [Int] -> m (a, ComputerState, Text)
launch program memory = run program (initial_state memory)

main :: IO ()
main = do
  memory :: [Int] <- fmap (read . unpack) . splitOn "," <$> getLine

  result <- eval (set_input 12 2 >> run_program >> get_output)
                 (initial_state memory)
  putStrLn $  "Solution 1: " <> show result

  results <- sequence $ eval_test (initial_state memory) <$> all_tests
  let sol = find fst $ zip results all_tests
  case sol of
    Just (_, (noun, verb)) ->
      putStrLn $  "Solution 2: " <> show (noun * 100 + verb)
    Nothing ->
      putStrLn "Couldn't find solution 2"
