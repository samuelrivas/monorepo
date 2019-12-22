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

shamelessParse :: ST s (STUArray s Int Int)
shamelessParse = factoryDeck 10007 >>= cut 1470
  >>= increment 46
  >>= cut (-6481)
  >>= increment 70
  >>= cut 547
  >>= increment 48
  >>= cut (-6479)
  >>= increment 69
  >>= cut (-5203)
  >>= increment 13
  >>= newStack
  >>= increment 73
  >>= newStack
  >>= cut (-6689)
  >>= increment 61
  >>= cut (-9853)
  >>= increment 48
  >>= cut (-9673)
  >>= newStack
  >>= increment 3
  >>= newStack
  >>= increment 64
  >>= cut 5789
  >>= increment 66
  >>= newStack
  >>= increment 70
  >>= cut (-2588)
  >>= increment 6
  >>= newStack
  >>= increment 6
  >>= cut (-7829)
  >>= increment 49
  >>= newStack
  >>= increment 19
  >>= cut 9777
  >>= newStack
  >>= increment 27
  >>= cut 6210
  >>= newStack
  >>= increment 12
  >>= cut 6309
  >>= increment 12
  >>= cut (-9458)
  >>= increment 5
  >>= cut 6369
  >>= increment 27
  >>= cut 2278
  >>= increment 42
  >>= cut 6656
  >>= increment 62
  >>= cut (-1424)
  >>= increment 25
  >>= newStack
  >>= increment 12
  >>= newStack
  >>= cut (-7399)
  >>= newStack
  >>= cut (-8925)
  >>= increment 47
  >>= newStack
  >>= cut 5249
  >>= increment 65
  >>= cut (-213)
  >>= newStack
  >>= cut 6426
  >>= increment 22
  >>= cut (-6683)
  >>= increment 38
  >>= newStack
  >>= increment 62
  >>= cut 6855
  >>= increment 75
  >>= cut 4965
  >>= newStack
  >>= cut (-5792)
  >>= increment 30
  >>= cut 9250
  >>= increment 19
  >>= cut (-948)
  >>= increment 26
  >>= cut (-5123)
  >>= increment 68
  >>= cut (-604)
  >>= increment 41
  >>= newStack
  >>= increment 45
  >>= cut 5572
  >>= newStack
  >>= cut 3853
  >>= increment 21
  >>= cut 1036
  >>= newStack
  >>= increment 6
  >>= cut 8114
  >>= newStack
  >>= increment 38
  >>= cut (-5)
  >>= increment 58
  >>= cut 9539
  >>= increment 19


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

increment :: Int -> STUArray s Int Int -> ST s (STUArray s Int Int)
increment n deck = do
  (lo, hi) <- getBounds deck

  let nElements = hi - lo + 1

  mapIndices
    (lo, hi)
    (\x ->
       let inv = modInv n nElements
       in ((x - lo) * inv) `mod` nElements)
    deck

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

