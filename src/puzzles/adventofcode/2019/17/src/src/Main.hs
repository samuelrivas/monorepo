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
                                        putStrLn, readFile, show, putStr)
import qualified Prelude

import System.Console.Readline (readline)
import Control.Monad.IO.Class (liftIO)
import           Control.Lens          (assign, at, modifying, set, use, view, _1, _2, productOf, both, ix)
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
import           Data.Text.IO          (putStrLn, readFile, putStr)
import Data.List (maximumBy, sort, tails)

import           Bidim
import           Intcode

type Scaffold = Map Coord Char

data Direction = Up | Down | Left | Right

show :: Show a => a -> Text
show = pack . Prelude.show

assert :: Bool -> ()
assert False = error "assertion failed"
assert True  = ()

encode :: (Num c, Enum a) => a -> c
encode = fromIntegral . fromEnum

decode :: (Integral c, Enum a) => c -> a
decode = toEnum . fromIntegral

intcodeToText :: [Integer] -> Text
intcodeToText = pack . fmap decode

textToIntcode :: Text -> [Integer]
textToIntcode = fmap encode . unpack

getInput :: IO [Integer]
getInput = fmap (read . unpack) . splitOn "," <$> readFile "input.txt"

getInput2 :: IO [Integer]
getInput2 = set (ix 0) 2 <$> getInput

readScaffold :: [Integer] -> Scaffold
readScaffold code =
  let
    out = intcodeToText . view _1 . runIdentity . eval (runProgram >> getOutput) $ code
    f (pos, scaffold) '\n' = (set _1 0 pos `plus` (0, 1), scaffold)
    f (pos, scaffold) c = (pos `plus` (1, 0), insert pos c scaffold)
  in
    view _2 . foldl' f ((0 ,0), empty) $ unpack out

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

findPath :: Scaffold -> Text
findPath = undefined

-- nextStep :: Scaffold -> Pos -> Direction -> Char
-- nextStep = undefined 

solution1 :: Scaffold -> Int
solution1 = sum . fmap (productOf both) . findCrosses

userLoop :: IntcodeT IO ()
userLoop = do
  runProgram
  output <- getOutput
  flushOutput
  liftIO . putStr $ intcodeToText output
  getStatus >>= \case
    Running -> userLoop
    Finished -> liftIO . putStrLn $ "finished"
    Aborted -> liftIO . putStrLn $ "aborted"
    Interrupted -> do
      Just input <- fmap pack <$> (liftIO . readline $ "$ ")
      pushInput $ textToIntcode (input <> "\n")
      userLoop

main :: IO ()
main = do
  code <- getInput
  let scaffold = readScaffold code

  putStrLn $ showBindim formatMap scaffold

  putStrLn $ "Solution 1: " <> show (solution1 scaffold)
  putStrLn $ "Solution 2: "


--

compose2 :: (b -> b -> c) -> (a -> b) -> (a -> a -> c)
compose2 g f x y = g (f x) (f y)

attach :: (a -> b) -> [a] -> [(b,a)]
attach key = map (\x -> (key x, x))

aux ::
   (((key, a) -> (key, a) -> b) -> [(key, a)] -> c) ->
      (key -> key -> b) -> (a -> key) ->
          ([a] -> c)
aux listFunc cmpFunc key =
   listFunc (compose2 cmpFunc fst) . attach key

maximum' :: Ord b => (a -> b) -> [a] -> a
maximum' key  =  snd . aux maximumBy compare key

lds :: Ord a => [a] -> [a]
lds = maximum' length . mapAdjacent lcp . sort . tails where
    lcp (x:xs) (y:ys) | x == y = x : lcp xs ys
    lcp _      _               = []

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f xs = zipWith f xs (tail xs)
