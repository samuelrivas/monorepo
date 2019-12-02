{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE OverloadedStrings  #-}

import           Prelude              hiding (getLine)

import           Control.Lens         (assign, modifying, use, uses)
import           Control.Monad.Loops  (whileM_)
import           Control.Monad.State
import           Data.Array           (elems, (!), (//))
import           Data.Generics.Labels ()
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
parse_opcode _ = Nothing

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

get_memory :: Monad m => ProgramT m [Int]
get_memory = uses #memory elems

restore :: Monad m => ProgramT m ()
restore = modifying #memory (// [(1, 12), (2, 2)])

main :: IO ()
main = do
  memory <- fmap (read . unpack) . splitOn "," <$> getLine
  end_memory <- evalStateT (restore >> run_program >> get_memory) (initial_state memory)
  print (head end_memory)
