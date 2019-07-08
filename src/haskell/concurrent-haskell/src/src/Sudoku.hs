import           Control.DeepSeq             (NFData, force)
import           Control.Parallel.Strategies (Eval, rpar, rseq, runEval)
import           Data.List                   (splitAt)
import           Data.Maybe                  (isJust)
import           Lib.Sudoku                  (solve)
import           System.Environment          (getArgs)

{-# ANN module "HLint: ignore Use camelCase" #-}

-- par_map :: (a -> b) -> [a] -> Eval [b]
-- par_map _ [] = pure []
-- par_map f (h:t) = do
--   h' <- rpar $ f h
--   t' <- par_map f t
--   pure $ h' : t'

par_map :: (a -> b) -> [a] -> Eval [b]
par_map f = traverse (rpar . f)

par_eval :: (NFData a, NFData b) => (a, b) -> (a, b)
par_eval (left, right) = runEval $ do
  a <- rpar $ force left
  b <- rpar $ force right
  rseq a
  rseq b
  return (a, b)

main :: IO ()
main = do
  [f] <- getArgs
  input <- readFile f
  let puzzles = runEval . rseq . force . lines $ input
      solutions = runEval (par_map solve puzzles)
  print . length . filter isJust $ solutions
