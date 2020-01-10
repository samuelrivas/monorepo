import System.Environment (getArgs)
import qualified Data.HashMap as HashMap
import Data.HashMap (Map)

import qualified Advent.Day2
import qualified Advent.Day3
import qualified Advent.Day4

-- FIXME: Run each argument separately so that we can report errors independendly.
-- Like "2: solution ...
--      "foo: not found"

-- FIXME: Running without arguments should run all of them

dispatcher :: Map String (IO ())
dispatcher =
  HashMap.fromList
  [("2", Advent.Day2.main),
   ("3", Advent.Day3.main),
   ("4", Advent.Day4.main)
  ]

dispatch :: [String] -> Maybe [IO ()]
dispatch = traverse (`HashMap.lookup` dispatcher)

main :: IO ()
main = do
  args <- getArgs
  case dispatch args of
    Just x -> sequence_ x
    Nothing -> putStrLn "invalid arguments"
