import           Data.HashMap.Lazy  (HashMap)
import qualified Data.HashMap.Lazy  as HashMap
import           System.Environment (getArgs)

import qualified Advent.Day1
import qualified Advent.Day10
import qualified Advent.Day11
import qualified Advent.Day12
import qualified Advent.Day2
import qualified Advent.Day3
import qualified Advent.Day4
import qualified Advent.Day5
import qualified Advent.Day6
import qualified Advent.Day7
import qualified Advent.Day8
import qualified Advent.Day9

-- FIXME: Run each argument separately so that we can report errors
-- independendly.  Like "2: solution ...  "foo: not found"

-- FIXME: Running without arguments should run all of them

dispatcher :: HashMap String (IO ())
dispatcher =
  HashMap.fromList
  [("1", Advent.Day1.main),
   ("2", Advent.Day2.main),
   ("3", Advent.Day3.main),
   ("4", Advent.Day4.main),
   ("5", Advent.Day5.main),
   ("6", Advent.Day6.main),
   ("7", Advent.Day7.main),
   ("8", Advent.Day8.main),
   ("9", Advent.Day9.main),
   ("10", Advent.Day10.main),
   ("11", Advent.Day11.main),
   ("12", Advent.Day12.main)
  ]

dispatch :: [String] -> Maybe [IO ()]
dispatch = traverse (`HashMap.lookup` dispatcher)

main :: IO ()
main = do
  args <- getArgs
  case dispatch args of
    Just x  -> sequence_ x
    Nothing -> putStrLn "invalid arguments"
