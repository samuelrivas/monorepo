import System.Environment (getArgs)
import qualified Data.HashMap as HashMap
import Data.HashMap (Map)

import qualified Advent.Day2
import qualified Advent.Day3
import qualified Advent.Day4
import qualified Advent.Day5
import qualified Advent.Day6
import qualified Advent.Day7
import qualified Advent.Day8
import qualified Advent.Day9
import qualified Advent.Day10
import qualified Advent.Day11
import qualified Advent.Day12
import qualified Advent.Day13
import qualified Advent.Day14
import qualified Advent.Day15
import qualified Advent.Day16
import qualified Advent.Day17
import qualified Advent.Day18
import qualified Advent.Day19
import qualified Advent.Day20
import qualified Advent.Day21
import qualified Advent.Day22
import qualified Advent.Day23
import qualified Advent.Day24
import qualified Advent.Day25

-- FIXME: Run each argument separately so that we can report errors independendly.
-- Like "2: solution ...
--      "foo: not found"

-- FIXME: Running without arguments should run all of them

dispatcher :: Map String (IO ())
dispatcher =
  HashMap.fromList
  [("2", Advent.Day2.main),
   ("3", Advent.Day3.main),
   ("4", Advent.Day4.main),
   ("5", Advent.Day5.main),
   ("6", Advent.Day6.main),
   ("7", Advent.Day7.main),
   ("8", Advent.Day8.main),
   ("9", Advent.Day9.main),
   ("10", Advent.Day10.main),
   ("11", Advent.Day11.main),
   ("12", Advent.Day12.main),
   ("13", Advent.Day13.main),
   ("14", Advent.Day14.main),
   ("15", Advent.Day15.main),
   ("16", Advent.Day16.main),
   ("17", Advent.Day17.main),
   ("18", Advent.Day18.main),
   ("19", Advent.Day19.main),
   ("20", Advent.Day20.main),
   ("21", Advent.Day21.main),
   ("22", Advent.Day22.main),
   ("23", Advent.Day23.main),
   ("24", Advent.Day24.main),
   ("25", Advent.Day25.main)
  ]

dispatch :: [String] -> Maybe [IO ()]
dispatch = traverse (`HashMap.lookup` dispatcher)

main :: IO ()
main = do
  args <- getArgs
  case dispatch args of
    Just x -> sequence_ x
    Nothing -> putStrLn "invalid arguments"
