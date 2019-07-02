import           Control.DeepSeq             (NFData, force)
import           Control.Parallel.Strategies (rpar, rseq, runEval)
import           Data.List                   (splitAt)
import           Data.Maybe                  (isJust)
import           Lib.Sudoku                  (solve)
import           System.Environment          (getArgs)

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
  let puzzles = lines input
      (left, right) = splitAt (length puzzles `div` 2) puzzles
      (solutions_left, solutions_right) = par_eval (solve <$> left, solve <$> right)
  print . length . filter isJust $ solutions_left
  print . length . filter isJust $ solutions_right
