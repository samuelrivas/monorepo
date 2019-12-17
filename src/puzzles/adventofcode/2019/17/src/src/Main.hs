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
{-# LANGUAGE ScopedTypeVariables   #-}

import           Prelude               hiding (Left, Right, concat, getLine,
                                        putStrLn, readFile, show)
import qualified Prelude

import           Control.Lens          (assign, at, modifying, set, use, view, _1, _2, productOf, both)
import           Control.Monad         (when)
import           Control.Monad.Loops   (untilJust)
import           Control.Monad.RWS.CPS (RWST, evalRWST, execRWST, lift, tell)
import           Data.Foldable         (fold, foldl')
import           Data.Functor.Identity (runIdentity)
import           Data.Generics.Labels  ()
import           Data.Map.Strict       (Map, empty, insert, keys)
import           Data.Maybe            (isNothing)
import           Data.Sequence         (Seq ((:<|)), fromList, (><), (|>))
import qualified Data.Sequence         as Seq
import           Data.Text             (Text, pack, splitOn, unpack)
import           Data.Text.IO          (putStrLn, readFile)

import           Bidim
import           Intcode

type Scaffold = Map Coord Char

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

intToChar :: Integer -> Char
intToChar = toEnum . fromIntegral

readScaffold :: [Integer] -> Scaffold
readScaffold code =
  let
    out = fmap intToChar . view _1 . runIdentity . eval (runProgram >> getOutput) $ code
    f (pos, scaffold) '\n' = (set _1 0 pos `plus` (0, 1), scaffold)
    f (pos, scaffold) c = (pos `plus` (1, 0), insert pos c scaffold)
  in
    view _2 . foldl' f ((0 ,0), empty) $ out

formatMap :: Maybe Char -> Text
formatMap Nothing = "?"
formatMap (Just c) = pack [c]

isCross :: Scaffold -> Coord -> Bool
isCross scaffold coord =
  let
    cross = [ coord,
              coord `plus` (0, 1),
             coord `plus` (0, -1),
             coord `plus` (-1, 0),
             coord `plus` (1, 0)]
  in
    maybe False (all (== '#'))
    $ mapM (\pos -> view (at pos) scaffold) cross

findCrosses :: Scaffold -> [Coord]
findCrosses scaffold = filter (isCross scaffold) $ keys scaffold

solution1 :: Scaffold -> Int
solution1 = sum . fmap (productOf both) . findCrosses

main :: IO ()
main = do
  code <- getInput
  let scaffold = readScaffold code

  putStrLn $ showBindim formatMap scaffold

  putStrLn $ "Solution 1: " <> show (solution1 scaffold)
  putStrLn $ "Solution 2: "
