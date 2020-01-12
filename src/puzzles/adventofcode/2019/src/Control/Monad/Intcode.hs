{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Control.Monad.Intcode (
  IntcodeState,
  Status (..),
  IntcodeT,
  abort,
  asciiTerminal,
  codeForDay,
  eval,
  exec,
  flushOutput,
  getOutput,
  getStatus,
  initialState,
  intcodeToText,
  pushInput,
  reset,
  runEmpty,
  run,
  runProgram,
  runWithOutput,
  save,
  stepProgram,
  textToIntcode,
  trace
  ) where

import           Prelude                        hiding (getLine, putStr,
                                                 putStrLn)

import           Control.Lens                   (assign, at, ix, modifying, non,
                                                 preview, use, uses, view)
import           Control.Monad                  (when)
import           Control.Monad.Fail             (MonadFail)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Loops            (whileM_)
import           Control.Monad.Reader           (MonadReader)
import           Control.Monad.RWS.CPS          (RWST, evalRWST, execRWST, get,
                                                 put, runRWST, tell)
import           Control.Monad.State            (MonadState)
import           Control.Monad.Trans.Class      (MonadTrans)
import           Control.Monad.Writer           (MonadWriter)
import           Data.Generics.Labels           ()
import           Data.List                      (uncons)
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text, pack, splitOn, unpack)
import           Data.Text.IO                   (putStr, putStrLn)
import           System.Console.Readline        (readline)

import           Control.Monad.Intcode.Internal
import           System.IO.Advent               (getInput)

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
getMode pos = fromMaybe Position . preview (ix . fromIntegral $ pos)

get2Modes :: [Mode] -> (Mode, Mode)
get2Modes modes =
  let getMode' = flip getMode modes
  in (getMode' 0, getMode' 1)

get3Modes :: [Mode] -> (Mode, Mode, Mode)
get3Modes modes =
  let getMode' = flip getMode modes
  in (getMode' 0, getMode' 1, getMode' 2)

parseOpcode :: Integer -> [Mode] -> Maybe Opcode
parseOpcode 1 modes   = Just . Add $ get3Modes modes
parseOpcode 2 modes   = Just . Mul $ get3Modes modes
parseOpcode 3 modes   = Just . In $ getMode 0 modes
parseOpcode 4 modes   = Just . Out $ getMode 0 modes
parseOpcode 5 modes   = Just . JumpTrue $ get2Modes modes
parseOpcode 6 modes   = Just . JumpFalse $ get2Modes modes
parseOpcode 7 modes   = Just . LessThan $ get3Modes modes
parseOpcode 8 modes   = Just . Equals $ get3Modes modes
parseOpcode 9 modes   = Just . AdjustBase $ getMode 0 modes
parseOpcode 99 _modes = Just Halt
parseOpcode _ _       = Nothing

parseMode :: Integer -> Maybe Mode
parseMode 0 = Just Position
parseMode 1 = Just Immediate
parseMode 2 = Just Relative
parseMode _ = Nothing

factor :: Integer -> [Integer]
factor 0 = []
factor x = factor (x `div` 10) ++ [x `mod` 10]

parseInstructionValue :: Integer -> Maybe Opcode
parseInstructionValue value =
  let
    int_opcode = value `mod` 100
    int_modes = reverse . factor $ value `div` 100
  in sequence (parseMode <$> int_modes) >>= parseOpcode int_opcode

popOpcode :: Monad m => IntcodeT m (Maybe Opcode)
popOpcode = parseInstructionValue <$> popValue

readMemory :: Monad m => Integer -> IntcodeT m Integer
readMemory pos = uses #memory (view (at pos . non 0))

writeMemory :: Monad m => Integer -> Integer -> IntcodeT m ()
writeMemory value position = assign (#memory . at position) $ Just value

abort :: Monad m => Text -> IntcodeT m ()
abort msg = do
  tell $ "Something went wrong: " <> msg <> "\n"
  tell ">>>>>>>> Dump\n"
  get >>= tell . pack . show
  assign #status Aborted

runOpcode :: Monad m => Opcode -> IntcodeT m ()
runOpcode Halt        = assign #status Finished
runOpcode (Add modes) = runArith (+) modes
runOpcode (Mul modes) = runArith (*) modes
runOpcode (JumpTrue modes) = jumpOn (/= 0) modes
runOpcode (JumpFalse modes) = jumpOn (== 0) modes
runOpcode (LessThan modes) = setBool (<) modes
runOpcode (Equals modes) = setBool (==) modes

runOpcode (AdjustBase mode) = do
  value <- readValue mode
  modifying #base (+ value)

runOpcode (In mode) =
  uses #input uncons >>= \case
  Just (h, t) -> do
    readDestination mode >>= writeMemory h
    assign #input t
  Nothing -> do
    modifying #pp (+ (-1))
    assign #status Interrupted

runOpcode (Out mode) = do
  value <- readValue mode
  modifying #output (value:)

jumpOn :: Monad m => (Integer -> Bool) -> (Mode, Mode) -> IntcodeT m ()
jumpOn p (mode_1, mode_2) = do
  test <- readValue mode_1
  dest <- readValue mode_2
  when (p test) $ assign #pp dest

setBool :: Monad m =>
  (Integer -> Integer -> Bool) -> (Mode, Mode, Mode) -> IntcodeT m ()
setBool p (mode_1, mode_2, mode_3) = do
  x <- readValue mode_1
  y <- readValue mode_2
  dest <- readDestination mode_3
  writeMemory (fromIntegral . fromEnum $ p x y) dest

runArith :: Monad m =>
  (Integer -> Integer -> Integer) -> (Mode, Mode, Mode) -> IntcodeT m ()
runArith op (mode_1, mode_2, mode_3) = do
  x <- readValue mode_1
  y <- readValue mode_2
  readDestination mode_3 >>= writeMemory (op x y)

-- Read the next value in memory, and advance program counter
popValue :: Monad m => IntcodeT m Integer
popValue = do
  value <- use #pp >>= readMemory
  modifying #pp (+1)
  pure value

readValue :: Monad m => Mode -> IntcodeT m Integer
readValue Position  = popValue >>= readMemory
readValue Immediate = popValue
readValue Relative = do
  value <- popValue
  uses #base (+ value) >>= readMemory

readDestination :: Monad m => Mode -> IntcodeT m Integer
readDestination Position = readValue Immediate
readDestination Immediate = do
  abort "Trying to write to immediate position"
  pure (-1)
readDestination Relative = do
  pos <- readValue Immediate
  uses #base (+ pos)

-- Public interface

stepProgram :: Monad m => IntcodeT m ()
stepProgram =
  popOpcode >>= \case
    Just opc -> runOpcode opc
    Nothing -> abort "could not pop next opcode"

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

-- Adventofcode utils
codeForDay :: MonadIO m => String -> m [Integer]
codeForDay day = fmap (read . unpack) . splitOn "," <$> getInput day

-- ASCII Interface
encode :: (Num c, Enum a) => a -> c
encode = fromIntegral . fromEnum

decode :: (Integral c, Enum a) => c -> a
decode = toEnum . fromIntegral

intcodeToText :: [Integer] -> Text
intcodeToText = pack . fmap decode

textToIntcode :: Text -> [Integer]
textToIntcode = fmap encode . unpack

asciiTerminal :: MonadFail m => MonadIO m => IntcodeT m ()
asciiTerminal = do
  runProgram
  output <- getOutput
  flushOutput
  liftIO . putStr $ intcodeToText output
  getStatus >>= \case
    Running -> asciiTerminal
    Finished -> liftIO . putStrLn $ "finished"
    Aborted -> liftIO . putStrLn $ "aborted"
    Interrupted -> do
      Just input <- fmap pack <$> (liftIO . readline $ "$ ")
      pushInput $ textToIntcode (input <> "\n")
      asciiTerminal
