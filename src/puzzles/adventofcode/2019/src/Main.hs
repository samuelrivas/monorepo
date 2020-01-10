import System.Environment (getArgs)
import qualified Data.HashMap as HashMap
import Data.HashMap (Map)

import qualified Advent.Day2 as Day2

-- FIXME: Run each argument separately so that we can report errors independendly.
-- Like "2: solution ...
--      "foo: not found"

-- FIXME: Running without arguments should run all of them

day2 :: IO ()
day2 = Day2.main

dispatcher :: Map String (IO ())
dispatcher =
  HashMap.fromList
  [("2", day2)]

dispatch :: [String] -> Maybe [IO ()]
dispatch = traverse (`HashMap.lookup` dispatcher)

main :: IO ()
main = do
  args <- getArgs
  case dispatch args of
    Just x -> sequence_ x
    Nothing -> putStrLn "invalid arguments"
