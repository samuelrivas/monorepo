-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

import           Prelude                hiding (lines, putStrLn, readFile, show,
                                         unlines)
import qualified Prelude

import           Control.Lens           (at, ix, over, preview, set, view,
                                         views, _1, _2)
import           Control.Monad          (replicateM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (Reader)
import           Data.Char              (isAsciiLower, isAsciiUpper, toLower)
import           Data.Foldable          (find)
import           Data.Functor.Identity  (runIdentity)
import           Data.Generics.Labels   ()
import  Data.HashSet          (HashSet)
import qualified Data.HashSet           as HashSet
import qualified Data.Map.Strict        as Map
import qualified Data.Text as Text
import           Data.Maybe             (catMaybes, fromJust)
import           Data.Text              (Text, pack)
import           Data.Text.IO           (putStrLn, readFile)
import Control.Monad.State
import Data.Bool (bool)

import           Bidim

show :: Show a => a -> Text
show = pack . Prelude.show

solve1 :: Text -> IO ()
solve1 text =
  let eris = parseInput text
  in do
    dup <- evalStateT (findDup eris) HashSet.empty
    putStrLn "input:"
    putStrLn $ showBidim showCell eris
    putStrLn "First dup:"
    putStrLn $ showBidim showCell dup

solve2 :: Text -> IO ()
solve2 text = undefined

getInput :: IO Text
getInput = readFile "input.txt"

parseInput :: Text -> Bidim Bool
parseInput = Map.map (== '#') . fromText

showCell :: Maybe Bool -> Text
showCell = maybe "?" (bool "." "#")

readCell :: Bidim Bool -> Coord -> Bool
readCell eris p = Map.findWithDefault False p eris

updateCell :: Bidim Bool -> Coord -> Bool -> Bool
updateCell eris pos isBug =
  let
    neighbors = readCell eris <$> cross pos
    numBugs = sum $ fromEnum <$> neighbors
  in
    case (numBugs, isBug) of
      (n, True) | n /= 1 -> False
      (n, False) | 1 <= n && n <= 2 -> True
      (_, _) -> isBug

minute :: Bidim Bool -> Bidim Bool
minute eris = Map.mapWithKey (updateCell eris) eris

type ErisT = StateT (HashSet Text)

findDup :: Monad m => Bidim Bool -> ErisT m (Bidim Bool)
findDup eris =
  let
    printedEris = showBidim showCell eris
  in do
    seen <- gets $ HashSet.member printedEris
    if seen
      then pure eris
      else do
        modify (HashSet.insert printedEris)
        findDup (minute eris)

main :: IO ()
main = do
  input <- getInput
  solve1 input
  solve2 input
