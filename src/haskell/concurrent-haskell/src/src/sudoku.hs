import           Data.Maybe         (isJust)
import           Lib.Sudoku
import           System.Environment (getArgs)

main :: IO ()
main = do
  [f] <- getArgs
  input <- readFile f
  let puzzles = lines input
      solutions = solve <$> puzzles
  print . length . filter isJust $ solutions
