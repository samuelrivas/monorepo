{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import           Prelude                 hiding (Left, Right, concat, getLine,
                                          putStr, putStrLn, readFile, show)
import qualified Prelude

import           Control.Applicative     ((<|>))
import           Control.Lens            (assign, at, both, ix, modifying,
                                          productOf, set, use, view, views, _1,
                                          _2)
import           Control.Monad           (when)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Loops     (untilJust)
import           Control.Monad.RWS.CPS   (RWST, evalRWST, execRWST, lift, tell)
import Control.Monad.State -- close this
import           Data.Foldable           (fold, foldl')
import           Data.Functor.Identity   (runIdentity)
import           Data.Generics.Labels    ()
import           Data.List               (find, maximumBy, sort, tails)
import           Data.Map.Strict         (Map, empty, insert, keys, toList)
import           Data.Sequence           (Seq ((:<|)), fromList, (><), (|>))
import qualified Data.Sequence           as Seq
import           Data.Text               (Text, pack, splitOn, unpack, lines)
import qualified           Data.Text as Text
import           Data.Text.IO            (putStr, putStrLn, readFile)
import           System.Console.Readline (readline)

import           Intcode

show :: Show a => a -> Text
show = pack . Prelude.show

assert :: Bool -> ()
assert False = error "assertion failed"
assert True  = ()

encode :: (Num c, Enum a) => a -> c
encode = fromIntegral . fromEnum

decode :: (Integral c, Enum a) => c -> a
decode = toEnum . fromIntegral

getInput :: IO [Integer]
getInput = fmap (read . unpack) . splitOn "," <$> readFile "input.txt"

cleanComments :: Text -> Text
cleanComments = Text.unlines.  filter ((/=) '#' . Text.head) . Text.lines

getInstructions :: String -> IO [Integer]
getInstructions file = textToIntcode . cleanComments <$> readFile file

preRun :: String -> IntcodeT IO ()
preRun file = liftIO (getInstructions file) >>= pushInput

atCheckpoint :: IO IntcodeState
atCheckpoint =
  view _2 <$>
  (getInput >>=
   run (preRun "collect-all-and-go-to-checkpoint.txt" >>
       runProgram >>
       flushOutput))

solution1 :: [Integer] -> Int
solution1 = undefined

solution2 :: [Integer] -> Int
solution2 = undefined

main :: IO ()
main = do
  code <- getInput

  putStrLn $ "Solution 1: " <> show (solution1 code)
  putStrLn $ "Solution 2: " <> show (solution2 code)

