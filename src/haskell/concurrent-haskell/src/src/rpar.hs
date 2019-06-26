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

  -- We need to force test here, if we just returned it, or used it directly for
  -- that matter, then the difference between t1 and t0 would always be
  -- small. t2 would be either "fib large_number" after t0 or (fib large_number
  -- + fib small_number), depending on whether a is thronw in parallel before
  -- evaluating b or not
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

-- retuns immediately, evaluation takes around the time of "fib large_number"
test1 :: Eval (Integer, Integer)
test1 = do
  a <- rpar $ fib large_number
  b <- rpar $ fib small_number
  return (a, b)

-- returns after b, evaluation takes around as much as for test 1
test2 :: Eval (Integer, Integer)
test2 = do
  a <- rpar $ fib large_number
  b <- rseq $ fib small_number
  return (a, b)

-- returns after a, typically fully evaluated so evaluation is immediate
test3 :: Eval (Integer, Integer)
test3 = do
  a <- rpar $ fib large_number
  b <- rseq $ fib small_number
  _ <- rseq a
  return (a, b)

-- returns after evaluating a and b sequentially, Evaluation is immediate
test4 :: Eval (Integer, Integer)
test4 = do
  a <- rseq $ fib large_number
  b <- rseq $ fib small_number
  return (a, b)

-- returns after evaluating a, but without having evaluated b. Evaluation takes
-- as much as evaluating b does
test5 :: Eval (Integer, Integer)
test5 = do
  a <- rseq $ fib large_number
  b <- rpar $ fib small_number
  return (a, b)
