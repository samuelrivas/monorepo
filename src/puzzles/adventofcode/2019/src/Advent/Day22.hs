{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Advent.Day22 where

import           Prelude      hiding (Left, Right, concat, getLine, putStr,
                               putStrLn, readFile)

import           Data.Text    (Text)
import           Data.Text.IO (putStrLn, readFile)

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

shamelessParse' :: Integral i => i -> i -> i
shamelessParse' !nCards !x =
    increment' nCards 19
  . cut' nCards 9539
  . increment' nCards 58
  . cut' nCards (-5)
  . increment' nCards 38
  . newStack nCards
  . cut' nCards 8114
  . increment' nCards 6
  . newStack nCards
  . cut' nCards 1036
  . increment' nCards 21
  . cut' nCards 3853
  . newStack nCards
  . cut' nCards 5572
  . increment' nCards 45
  . newStack nCards
  . increment' nCards 41
  . cut' nCards (-604)
  . increment' nCards 68
  . cut' nCards (-5123)
  . increment' nCards 26
  . cut' nCards (-948)
  . increment' nCards 19
  . cut' nCards 9250
  . increment' nCards 30
  . cut' nCards (-5792)
  . newStack nCards
  . cut' nCards 4965
  . increment' nCards 75
  . cut' nCards 6855
  . increment' nCards 62
  . newStack nCards
  . increment' nCards 38
  . cut' nCards (-6683)
  . increment' nCards 22
  . cut' nCards 6426
  . newStack nCards
  . cut' nCards (-213)
  . increment' nCards 65
  . cut' nCards 5249
  . newStack nCards
  . increment' nCards 47
  . cut' nCards (-8925)
  . newStack nCards
  . cut' nCards (-7399)
  . newStack nCards
  . increment' nCards 12
  . newStack nCards
  . increment' nCards 25
  . cut' nCards (-1424)
  . increment' nCards 62
  . cut' nCards 6656
  . increment' nCards 42
  . cut' nCards 2278
  . increment' nCards 27
  . cut' nCards 6369
  . increment' nCards 5
  . cut' nCards (-9458)
  . increment' nCards 12
  . cut' nCards 6309
  . increment' nCards 12
  . newStack nCards
  . cut' nCards 6210
  . increment' nCards 27
  . newStack nCards
  . cut' nCards 9777
  . increment' nCards 19
  . newStack nCards
  . increment' nCards 49
  . cut' nCards (-7829)
  . increment' nCards 6
  . newStack nCards
  . increment' nCards 6
  . cut' nCards (-2588)
  . increment' nCards 70
  . newStack nCards
  . increment' nCards 66
  . cut' nCards 5789
  . increment' nCards 64
  . newStack nCards
  . increment' nCards 3
  . newStack nCards
  . cut' nCards (-9673)
  . increment' nCards 48
  . cut' nCards (-9853)
  . increment' nCards 61
  . cut' nCards (-6689)
  . newStack nCards
  . increment' nCards 73
  . newStack nCards
  . increment' nCards 13
  . cut' nCards (-5203)
  . increment' nCards 69
  . cut' nCards (-6479)
  . increment' nCards 48
  . cut' nCards 547
  . increment' nCards 70
  . cut' nCards (-6481)
  . increment' nCards 46
  . cut' nCards 1470 $ x

newStack :: Integral i => i -> i -> i
newStack !nCards !i = nCards - i - 1

cut :: Integral i => i -> i -> i -> i
cut nCards n i = (i + n) `mod` nCards

cut' :: Integral i => i -> i -> i -> i
cut' !nCards !n !i = (i - n) `mod` nCards

increment :: Integral i => i -> i -> i -> i
increment nCards n i =
  let inv = modInv n nCards
  in (i * inv) `mod` nCards

increment' :: Integral i => i -> i -> i -> i
increment' !nCards !n !i = (i * n) `mod` nCards

-- For a deck of n cards (n being prime) power x == power (n + x -1)
power :: Integral i => Int -> (i -> i) -> (i -> i)
power n f = foldl' (.) id $ replicate n f

modAbs :: Integral i => i -> i -> i
modAbs !x !modulus =
  let m = x `mod` modulus
  in if m < 0 then modulus + m else m

extendedEuclid :: Integral i => i -> i -> (i, i)
extendedEuclid _ 0 = (1, 0)
extendedEuclid !a !b =
  let
    q = a `div` b
    r = a `mod` b
    (x, y) = extendedEuclid b r
  in
    (y, x - q * y)

-- Returns 1 if there isn't an inv
modInv :: Integral i => i -> i -> i
modInv !n !modulus =
  let (a, _) = extendedEuclid (modAbs n modulus) modulus
  in modAbs a modulus

getInput :: IO Text
getInput = readFile "input.txt"


bigDeck :: Integral i => i
bigDeck = 119315717514047

period :: Integral i => i
period = bigDeck - 1

backwards :: Integral i => i
backwards = period - 101741582076661

strictLoop :: Integral i => i -> i -> i -> i
strictLoop !decksize !pos 1 = shamelessParse' decksize pos
strictLoop !decksize !pos (!n) = shamelessParse' decksize $! strictLoop decksize pos (n - 1)

main :: IO ()
main = do
  -- shuffle <- getInput
  putStrLn $ "Solution 1: done manually so far "
  putStrLn $ "Solution 2: takes forever :("
  -- putStrLn $ pack . show $! strictLoop bigDeck 2020 backwards
