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

shamelessParse :: Integral i => i -> i -> i
shamelessParse nCards =
    cut nCards 1470
  . increment nCards 46
  . cut nCards (-6481)
  . increment nCards 70
  . cut nCards 547
  . increment nCards 48
  . cut nCards (-6479)
  . increment nCards 69
  . cut nCards (-5203)
  . increment nCards 13
  . newStack nCards
  . increment nCards 73
  . newStack nCards
  . cut nCards (-6689)
  . increment nCards 61
  . cut nCards (-9853)
  . increment nCards 48
  . cut nCards (-9673)
  . newStack nCards
  . increment nCards 3
  . newStack nCards
  . increment nCards 64
  . cut nCards 5789
  . increment nCards 66
  . newStack nCards
  . increment nCards 70
  . cut nCards (-2588)
  . increment nCards 6
  . newStack nCards
  . increment nCards 6
  . cut nCards (-7829)
  . increment nCards 49
  . newStack nCards
  . increment nCards 19
  . cut nCards 9777
  . newStack nCards
  . increment nCards 27
  . cut nCards 6210
  . newStack nCards
  . increment nCards 12
  . cut nCards 6309
  . increment nCards 12
  . cut nCards (-9458)
  . increment nCards 5
  . cut nCards 6369
  . increment nCards 27
  . cut nCards 2278
  . increment nCards 42
  . cut nCards 6656
  . increment nCards 62
  . cut nCards (-1424)
  . increment nCards 25
  . newStack nCards
  . increment nCards 12
  . newStack nCards
  . cut nCards (-7399)
  . newStack nCards
  . cut nCards (-8925)
  . increment nCards 47
  . newStack nCards
  . cut nCards 5249
  . increment nCards 65
  . cut nCards (-213)
  . newStack nCards
  . cut nCards 6426
  . increment nCards 22
  . cut nCards (-6683)
  . increment nCards 38
  . newStack nCards
  . increment nCards 62
  . cut nCards 6855
  . increment nCards 75
  . cut nCards 4965
  . newStack nCards
  . cut nCards (-5792)
  . increment nCards 30
  . cut nCards 9250
  . increment nCards 19
  . cut nCards (-948)
  . increment nCards 26
  . cut nCards (-5123)
  . increment nCards 68
  . cut nCards (-604)
  . increment nCards 41
  . newStack nCards
  . increment nCards 45
  . cut nCards 5572
  . newStack nCards
  . cut nCards 3853
  . increment nCards 21
  . cut nCards 1036
  . newStack nCards
  . increment nCards 6
  . cut nCards 8114
  . newStack nCards
  . increment nCards 38
  . cut nCards (-5)
  . increment nCards 58
  . cut nCards 9539
  . increment nCards 19

-- factoryDeck :: MArray a Int m => Int -> m (a Int Int)
-- factoryDeck n = newListArray (0, n - 1) [0..n-1]

-- newStack :: STUArray s Int Int -> ST s (STUArray s Int Int)
-- newStack deck = do
--   (lo, hi) <- getBounds deck
--   mapIndices (lo, hi) (\x -> hi - x + lo) deck

newStack :: Integral i => i -> i -> i
newStack nCards i = nCards - i - 1

-- cut :: Int -> STUArray s Int Int -> ST s (STUArray s Int Int)
-- cut n deck = do
--   (lo, hi) <- getBounds deck
--   mapIndices (lo, hi) (\x -> (x - lo + n) `mod` (hi - lo + 1)) deck

cut :: Integral i => i -> i -> i -> i
cut nCards n i = (i + n) `mod` nCards
-- increment :: Int -> STUArray s Int Int -> ST s (STUArray s Int Int)
-- increment n deck = do
--   (lo, hi) <- getBounds deck

--   let nElements = hi - lo + 1

--   mapIndices
--     (lo, hi)
--     (\x ->
--        let inv = modInv n nElements
--        in ((x - lo) * inv) `mod` nElements)
--     deck

increment :: Integral i => i -> i -> i -> i
increment nCards n i =
  let inv = modInv n nCards
  in (i * inv) `mod` nCards

-- For a deck of n cards (n being prime) power x == power (n + x -1)
power :: Integral i => Int -> (i -> i) -> (i -> i)
power n f = foldr (.) id $ replicate n f

modAbs :: Integral i => i -> i -> i
modAbs x modulus =
  let m = x `mod` modulus
  in if m < 0 then modulus + m else m

extendedEuclid :: Integral i => i -> i -> (i, i)
extendedEuclid _ 0 = (1, 0)
extendedEuclid a b =
  let
    q = a `div` b
    r = a `mod` b
    (x, y) = extendedEuclid b r
  in
    (y, x - q * y)

-- Returns 1 if there isn't an inv
modInv :: Integral i => i -> i -> i
modInv n modulus =
  let (a, _) = extendedEuclid (modAbs n modulus) modulus
  in modAbs a modulus

getInput :: IO Text
getInput = readFile "input.txt"

main :: IO ()
main = do
  shuffle <- getInput
  putStrLn $ "Solution 1: "
  putStrLn $ "Solution 2: "

