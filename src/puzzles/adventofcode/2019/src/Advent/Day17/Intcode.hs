{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Advent.Day17.Intcode (
  IntcodeState,
  Status (..),
  IntcodeT,
  abort,
  eval,
  exec,
  flushOutput,
  getOutput,
  getStatus,
  initialState,
  pushInput,
  reset,
  runEmpty,
  run,
  runProgram,
  runWithOutput,
  save,
  stepProgram,
  trace
  ) where

import           Prelude                      hiding (getLine, putStrLn, show)

import           Control.Lens                 (assign, at, ix, modifying, non,
                                               preview, use, uses, view)
import           Control.Monad                (when)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Loops          (whileM_)
import           Control.Monad.Reader         (MonadReader)
import           Control.Monad.RWS.CPS        (RWST, evalRWST, execRWST, get,
                                               put, runRWST, tell)
import           Control.Monad.State          (MonadState)
import           Control.Monad.Trans.Class    (MonadTrans)
import           Control.Monad.Writer         (MonadWriter)
import           Data.Generics.Labels         ()
import           Data.List                    (uncons)
import           Data.Maybe                   (fromMaybe)
import           Data.Text                    (Text)

import           Advent.Day17.IntcodeInternal

newtype IntcodeT m a = IntcodeT { unIntcodeT :: RWST () Text IntcodeState m a }
  deriving newtype (Functor, Applicative, Monad, MonadWriter Text,
                    MonadState IntcodeState, MonadReader (), MonadTrans,
                    MonadIO, MonadFail)

eval :: Monad m => IntcodeT m a -> [Integer] -> m (a, Text)
eval program code = evalRWST (unIntcodeT program) () (initialState code)

run :: Monad m => IntcodeT m a -> [Integer] -> m (a, IntcodeState, Text)
run program code = runRWST (unIntcodeT program) () (initialState code)

-- Useful if you have a state saved to use with `reset`
runEmpty :: Monad m => IntcodeT m a -> m (a, IntcodeState, Text)
runEmpty = flip run []

exec :: Monad m => IntcodeT m a -> [Integer] -> m (IntcodeState, Text)
exec program code = execRWST (unIntcodeT program) () (initialState code)

getMode :: Integer -> [Mode] -> Mode
getMode pos = fromMaybe Position . preview (ix $ fromIntegral pos)

get_2_modes :: [Mode] -> (Mode, Mode)
get_2_modes modes =
  let getMode' = flip getMode modes
  in (getMode' 0, getMode' 1)

get_3_modes :: [Mode] -> (Mode, Mode, Mode)
get_3_modes modes =
  let getMode' = flip getMode modes
  in (getMode' 0, getMode' 1, getMode' 2)

parseOpcode :: Integer -> [Mode] -> Maybe Opcode
parseOpcode 1 modes   = Just . Add $ get_3_modes modes
parseOpcode 2 modes   = Just . Mul $ get_3_modes modes
parseOpcode 3 modes   = Just . In $ getMode 0 modes
parseOpcode 4 modes   = Just . Out $ getMode 0 modes
parseOpcode 5 modes   = Just . JumpTrue $ get_2_modes modes
parseOpcode 6 modes   = Just . JumpFalse $ get_2_modes modes
parseOpcode 7 modes   = Just . LessThan $ get_3_modes modes
parseOpcode 8 modes   = Just . Equals $ get_3_modes modes
parseOpcode 9 modes   = Just . AdjustBase $ getMode 0 modes
parseOpcode 99 _modes = Just Halt
parseOpcode _ _       = Nothing

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
  in sequence (parse_mode <$> int_modes) >>= parseOpcode int_opcode

pop_opcode :: Monad m => IntcodeT m (Maybe Opcode)
pop_opcode = parse_instruction_value <$> pop_value

read_memory :: Monad m => Integer -> IntcodeT m Integer
read_memory pos = uses #memory (view (at pos . non 0))

write_memory :: Monad m => Integer -> Integer -> IntcodeT m ()
write_memory value position = assign (#memory . at position) $ Just value

abort :: Monad m => Text -> IntcodeT m ()
abort msg = do
  tell $ "Something went wrong: " <> msg <> "\n"
  tell ">>>>>>>> Dump\n"
  get >>= tell . show
  assign #status Aborted

runOpcode :: Monad m => Opcode -> IntcodeT m ()
runOpcode Halt        = assign #status Finished
runOpcode (Add modes) = run_arith (+) modes
runOpcode (Mul modes) = run_arith (*) modes
runOpcode (JumpTrue modes) = jump_on (/= 0) modes
runOpcode (JumpFalse modes) = jump_on (== 0) modes
runOpcode (LessThan modes) = set_bool (<) modes
runOpcode (Equals modes) = set_bool (==) modes

runOpcode (AdjustBase mode) = do
  value <- read_value mode
  modifying #base (+ value)

runOpcode (In mode) =
  uses #input uncons >>= \case
  Just (h, t) -> do
    read_destination mode >>= write_memory h
    assign #input t
  Nothing -> do
    modifying #pp (+ (-1))
    assign #status Interrupted

runOpcode (Out mode) = do
  value <- read_value mode
  modifying #output (value:)

jump_on :: Monad m => (Integer -> Bool) -> (Mode, Mode) -> IntcodeT m ()
jump_on p (mode_1, mode_2) = do
  test <- read_value mode_1
  dest <- read_value mode_2
  when (p test) $ assign #pp dest

set_bool :: Monad m =>
  (Integer -> Integer -> Bool) -> (Mode, Mode, Mode) -> IntcodeT m ()
set_bool p (mode_1, mode_2, mode_3) = do
  x <- read_value mode_1
  y <- read_value mode_2
  dest <- read_destination mode_3
  write_memory (fromIntegral . fromEnum $ p x y) dest

run_arith :: Monad m =>
  (Integer -> Integer -> Integer) -> (Mode, Mode, Mode) -> IntcodeT m ()
run_arith op (mode_1, mode_2, mode_3) = do
  x <- read_value mode_1
  y <- read_value mode_2
  read_destination mode_3 >>= write_memory (op x y)

-- Read the next value in memory, and advance program counter
pop_value :: Monad m => IntcodeT m Integer
pop_value = do
  value <- use #pp >>= read_memory
  modifying #pp (+1)
  pure value

read_value :: Monad m => Mode -> IntcodeT m Integer
read_value Position  = pop_value >>= read_memory
read_value Immediate = pop_value
read_value Relative = do
  value <- pop_value
  uses #base (+ value) >>= read_memory

read_destination :: Monad m => Mode -> IntcodeT m Integer
read_destination Position = read_value Immediate
read_destination Immediate = do
  abort "Trying to write to immediate position"
  pure (-1)
read_destination Relative = do
  pos <- read_value Immediate
  uses #base (+ pos)

-- Public interface

stepProgram :: Monad m => IntcodeT m ()
stepProgram =
  pop_opcode >>= \case
    Just opc -> runOpcode opc
    Nothing  -> abort "could not pop next opcode"

runProgram :: Monad m => IntcodeT m ()
runProgram = whileM_ (uses #status (== Running)) stepProgram

runWithOutput :: Monad m => IntcodeT m [Integer]
runWithOutput = do
    whileM_
      ((&&) <$> uses #status (== Running) <*> uses #output (== []))
      stepProgram
    output <- getOutput
    flushOutput
    pure output

save :: Monad m => IntcodeT m IntcodeState
save = get

reset :: Monad m => IntcodeState -> IntcodeT m ()
reset = put

getOutput :: Monad m => IntcodeT m [Integer]
getOutput = uses #output reverse

flushOutput :: Monad m => IntcodeT m ()
flushOutput = assign #output []

trace :: Monad m => Text -> IntcodeT m ()
--trace = const $ pure ()
trace msg = tell $ "T>> " <> msg <> "\n"

getStatus :: Monad m => IntcodeT m Status
getStatus = use #status

pushInput :: Monad m => [Integer] -> IntcodeT m ()
pushInput x = do
  modifying #input (++ x)
  st <- use #status
  when (st == Interrupted) $ assign #status Running
