import           Control.Parallel.Strategies (Eval, rpar, rseq, runEval)
import           System.Environment          (getArgs)

main :: IO ()
main = do
  [n] <- getArgs
  let (_, _) = runEval $ [test1, test2, test3, test4] !! (read n - 1)
  return ()

ffib :: Integer -> Integer
ffib = (fibs !!) . fromInteger

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) (tail fibs) fibs

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

test1 :: Eval (Integer, Integer)
test1 = do
  a <- rpar $ fib 30
  b <- rpar $ fib 31
  return (a, b)

test2 :: Eval (Integer, Integer)
test2 = do
  a <- rpar $ fib 30
  b <- rpar $ fib 31
  return (a, b)

test3 :: Eval (Integer, Integer)
test3 = do
  a <- rpar $ fib 30
  b <- rpar $ fib 31
  return (a, b)

test4 :: Eval (Integer, Integer)
test4 = do
  a <- rpar $ fib 30
  b <- rpar $ fib 31
  return (a, b)
