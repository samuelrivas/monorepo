import           Control.Exception           (evaluate)
import           Control.Parallel.Strategies (Eval, rpar, rseq, runEval)
import           Data.Time.Clock             (diffUTCTime, getCurrentTime)
import           System.Environment          (getArgs)

{-# ANN module "HLint: ignore Use camelCase" #-}

main :: IO ()
main = do
  [n] <- getArgs
  t0 <- getCurrentTime
  let
    diffTime = flip diffUTCTime t0
    test = runEval $ [test1, test2, test3, test4, test5] !! (read n - 1)
  r <- evaluate test
  t1 <- getCurrentTime
  print r
  t2 <- getCurrentTime
  let elapsed_1 = diffTime t1
      elapsed_2 = diffTime t2
  print elapsed_1
  print elapsed_2

small_number :: Integer
small_number = 36

large_number :: Integer
large_number = 38

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

test1 :: Eval (Integer, Integer)
test1 = do
  a <- rpar $ fib large_number
  b <- rpar $ fib small_number
  return (a, b)

test2 :: Eval (Integer, Integer)
test2 = do
  a <- rpar $ fib large_number
  b <- rseq $ fib small_number
  return (a, b)

test3 :: Eval (Integer, Integer)
test3 = do
  a <- rpar $ fib large_number
  b <- rseq $ fib small_number
  _ <- rseq a
  return (a, b)

test4 :: Eval (Integer, Integer)
test4 = do
  a <- rseq $ fib large_number
  b <- rseq $ fib small_number
  return (a, b)

test5 :: Eval (Integer, Integer)
test5 = do
  a <- rseq $ fib large_number
  b <- rpar $ fib small_number
  return (a, b)
