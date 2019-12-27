{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Intcode (
  ComputerState,
  ProgramT,
  dump_memory,
  eval,
  exec,
  initial_state,
  launch,
  push_input,
  reset,
  run,
  run_program,
  step_program,
  ) where

import           Prelude               hiding (getLine, putStrLn)

import           Control.Lens          (assign, at, ix, modifying, non, preview,
                                        use, uses, view)
import           Control.Monad         (when)
import           Control.Monad.Loops   (whileM_)
import           Control.Monad.Reader  (MonadReader)
import           Control.Monad.RWS.CPS (RWST, evalRWST, execRWST, get, put,
                                        runRWST, tell)
import           Control.Monad.State   (MonadState)
import           Control.Monad.Writer  (MonadWriter)
import           Data.Generics.Labels  ()
import           Data.List             (uncons)
import           Data.Map.Strict       (assocs)
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text, pack)

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

get_mode :: Integer -> [Mode] -> Mode
get_mode pos = fromMaybe Position . preview (ix . fromIntegral $ pos)

get_2_modes :: [Mode] -> (Mode, Mode)
get_2_modes modes =
  let get_mode' = flip get_mode modes
  in (get_mode' 0, get_mode' 1)

get_3_modes :: [Mode] -> (Mode, Mode, Mode)
get_3_modes modes =
  let get_mode' = flip get_mode modes
  in (get_mode' 0, get_mode' 1, get_mode' 2)

parse_opcode :: Integer -> [Mode] -> Maybe Opcode
parse_opcode 1 modes   = Just . Add $ get_3_modes modes
parse_opcode 2 modes   = Just . Mul $ get_3_modes modes
parse_opcode 3 modes   = Just . In $ get_mode 0 modes
parse_opcode 4 modes   = Just . Out $ get_mode 0 modes
parse_opcode 5 modes   = Just . JumpTrue $ get_2_modes modes
parse_opcode 6 modes   = Just . JumpFalse $ get_2_modes modes
parse_opcode 7 modes   = Just . LessThan $ get_3_modes modes
parse_opcode 8 modes   = Just . Equals $ get_3_modes modes
parse_opcode 9 modes   = Just . AdjustBase $ get_mode 0 modes
parse_opcode 99 _modes = Just Halt
parse_opcode _ _       = Nothing

parse_mode :: Integer -> Maybe Mode
parse_mode 0 = Just Position
parse_mode 1 = Just Immediate
parse_mode 2 = Just Relative
parse_mode _ = Nothing

factor :: Integer -> [Integer]
factor 0 = []
factor x = factor (x `div` 10) ++ [x `mod` 10]

parse_instruction_value :: Integer -> Maybe Opcode
parse_instruction_value value =
  let
    int_opcode = value `mod` 100
    int_modes = reverse . factor $ value `div` 100
  in sequence (parse_mode <$> int_modes) >>= parse_opcode int_opcode

pop_opcode :: Monad m => ProgramT m (Maybe Opcode)
pop_opcode = parse_instruction_value <$> pop_value

read_memory :: Monad m => Integer -> ProgramT m Integer
read_memory pos = uses #memory (view (at pos . non 0))

write_memory :: Monad m => Integer -> Integer -> ProgramT m ()
write_memory value position = assign (#memory . at position) $ Just value

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
run_opcode (JumpTrue modes) = jump_on (/= 0) modes
run_opcode (JumpFalse modes) = jump_on (== 0) modes
run_opcode (LessThan modes) = set_bool (<) modes
run_opcode (Equals modes) = set_bool (==) modes

run_opcode (AdjustBase mode) = do
  value <- read_value mode
  modifying #base (+ value)

run_opcode (In mode) =
  uses #input uncons >>= \case
  Just (h, t) -> do
    read_destination mode >>= write_memory h
    assign #input t
  Nothing -> do
    modifying #pp (+ (-1))
    assign #status Interrupted

run_opcode (Out mode) = do
  value <- read_value mode
  tell $ pack (show value) <> "\n"

jump_on :: Monad m => (Integer -> Bool) -> (Mode, Mode) -> ProgramT m ()
jump_on p (mode_1, mode_2) = do
  test <- read_value mode_1
  dest <- read_value mode_2
  when (p test) $ assign #pp dest

set_bool :: Monad m =>
  (Integer -> Integer -> Bool) -> (Mode, Mode, Mode) -> ProgramT m ()
set_bool p (mode_1, mode_2, mode_3) = do
  x <- read_value mode_1
  y <- read_value mode_2
  dest <- read_destination mode_3
  write_memory (fromIntegral . fromEnum $ p x y) dest

run_arith :: Monad m =>
  (Integer -> Integer -> Integer) -> (Mode, Mode, Mode) -> ProgramT m ()
run_arith op (mode_1, mode_2, mode_3) = do
  x <- read_value mode_1
  y <- read_value mode_2
  read_destination mode_3 >>= write_memory (op x y)

-- Read the next value in memory, and advance program counter
pop_value :: Monad m => ProgramT m Integer
pop_value = do
  value <- use #pp >>= read_memory
  modifying #pp (+1)
  pure value

read_value :: Monad m => Mode -> ProgramT m Integer
read_value Position  = pop_value >>= read_memory
read_value Immediate = pop_value
read_value Relative = do
  value <- pop_value
  uses #base (+ value) >>= read_memory

read_destination :: Monad m => Mode -> ProgramT m Integer
read_destination Position = read_value Immediate
read_destination Immediate = do
  abort "Trying to write to immediate position"
  pure (-1)
read_destination Relative = do
  pos <- read_value Immediate
  uses #base (+ pos)

-- Public interface

step_program :: Monad m => ProgramT m ()
step_program =
  pop_opcode >>= \case
    Just opc -> run_opcode opc
    Nothing -> abort "could not pop next opcode"

run_program :: Monad m => ProgramT m ()
run_program = whileM_ (uses #status (== Running)) step_program

reset :: Monad m => [Integer] -> ProgramT m ()
reset = put . initial_state

dump_memory :: Monad m => ProgramT m [(Integer, Integer)]
dump_memory = uses #memory assocs

push_input :: Monad m => [Integer] -> ProgramT m ()
push_input x = do
  modifying #input (++ x)
  st <- use #status
  when (st == Interrupted) $ assign #status Running

launch :: Monad m => ProgramT m a -> [Integer] -> m (a, ComputerState, Text)
launch program memory = run program (initial_state memory)
