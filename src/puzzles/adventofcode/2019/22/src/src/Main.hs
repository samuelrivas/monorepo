{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE RankNTypes    #-}
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
import           Data.Maybe              (fromJust, isNothing)
import           Data.Sequence           (Seq ((:<|)), fromList, (><), (|>))
import qualified Data.Sequence           as Seq
import           Data.Text               (Text, pack, splitOn, unpack)
import           Data.Text.IO            (putStr, putStrLn, readFile)
import           System.Console.Readline (readline)
import Data.Array.ST
-- import Data.Array
import Data.Array.Unboxed (UArray)
import Data.Array.IArray (IArray)
import qualified Data.Array.IArray as IArray
import  Data.Array.MArray (MArray)
import qualified Data.Array.MArray as MArray
import Control.Monad.ST

type Deck s = STUArray s Int Int

factoryDeck :: MArray a Int m => Int -> m (a Int Int)
factoryDeck n = newListArray (0, n - 1) [0..n-1]

newStack :: STUArray s Int Int -> ST s (STUArray s Int Int)
newStack deck = do
  (lo, hi) <- getBounds deck
  mapIndices (lo, hi) (\x -> hi - x + lo) deck

cut :: Int -> STUArray s Int Int -> ST s (STUArray s Int Int)
cut n deck = do
  (lo, hi) <- getBounds deck
  mapIndices (lo, hi) (\x -> (x - lo + n) `mod` (hi - lo + 1)) deck

getInput :: IO Text
getInput = readFile "input.txt"

-- toList :: forall s.ST s (STUArray s Int Int) -> [Int]
-- toList  = (IArray.elems :: UArray Int Int -> [Int]) .
--           (runSTUArray :: ST s (STUArray s Int Int) -> UArray Int Int)

-- toList :: (forall s.ST s (STUArray s Int Int)) -> [Int]
-- toList = IArray.elems . (runSTUArray :: ST s (STUArray s Int Int) -> UArray Int Int)

main :: IO ()
main = do
  shuffle <- getInput
  putStrLn $ "Solution 1: "
  putStrLn $ "Solution 2: "

