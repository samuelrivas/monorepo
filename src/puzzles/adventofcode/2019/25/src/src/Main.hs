-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import           Prelude                hiding (Left, Right, concat, getLine,
                                         putStr, putStrLn, readFile, show,
                                         unlines)
import qualified Prelude

import           Control.Lens           (view, _1, _2)
import           Control.Monad.IO.Class (liftIO)
import           Data.Functor.Identity  (runIdentity)
import           Data.Generics.Labels   ()
import           Data.Hashable          (Hashable)
import           Data.HashSet           (HashSet)
import qualified Data.HashSet           as HashSet
import           Data.List              (find, subsequences)
import           Data.Text              (Text, pack, splitOn, unlines, unpack)
import qualified Data.Text              as Text
import           Data.Text.IO           (putStrLn, readFile)

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

-- Found manually
atCheckpoint :: IO IntcodeState
atCheckpoint =
  view _2 <$>
  (getInput >>=
   run (preRun "collect-all-and-go-to-checkpoint.txt" >>
       runProgram >>
       flushOutput))

-- Explored manually
inventory :: HashSet Text
inventory = HashSet.fromList [
  "easter egg",
  "mug",
  "sand",
  "weather machine",
  "festive hat",
  "shell",
  "whirled peas",
  "space heater"]

dropItems :: Monad m => HashSet Text -> IntcodeT m ()
dropItems dropSet =
  let commands = unlines . HashSet.toList . HashSet.map ("drop " <>) $ dropSet
  in pushInput . textToIntcode $ commands

testInventory :: Monad m => HashSet Text -> IntcodeT m Text
testInventory dropSet = do
  dropItems dropSet
  pushInput . textToIntcode $ "south\n"
  runProgram
  output <- intcodeToText <$> getOutput
  flushOutput
  pure output

checkDropSet :: Monad m => IntcodeState -> HashSet Text -> IntcodeT m Bool
checkDropSet checkpoint dropSet =
  isCorrect <$> (reset checkpoint >> testInventory dropSet)

isCorrect :: Text -> Bool
isCorrect output =
  let p = flip Text.isInfixOf output
  in not (p alertLighter || p alertHeavier)

allSubsets :: Eq a => Hashable a => HashSet a -> [HashSet a]
allSubsets s = HashSet.fromList <$> (subsequences . HashSet.toList $ s)

alertLighter :: Text
alertLighter = "Alert! Droids on this ship are lighter than the detected value!"

alertHeavier :: Text
alertHeavier = "Alert! Droids on this ship are heavier than the detected value!"

findCorrectDropSet :: IntcodeState -> Maybe (HashSet Text)
findCorrectDropSet checkpoint =
  find (view _1 . runIdentity. runEmpty . checkDropSet checkpoint) $
    allSubsets inventory

-- Found by testing drop sets from the checkpoint
correctInventory :: [Text]
correctInventory = ["easter egg", "mug", "sand", "space heater"]

solution1 :: [Integer] -> IO Text
solution1 code =
  let
    instructions = intcodeToText <$>
                   (preRun "solution1.txt" >> runProgram >> getOutput)
  in
    view _1 <$> run instructions code

solution2 :: [Integer] -> Int
solution2 = undefined

main :: IO ()
main = do
  code <- getInput

  putStrLn "Solution 1: (Read below)"
  solution1 code >>= putStrLn
  putStrLn $ "Solution 2: " <> "not yet :("
