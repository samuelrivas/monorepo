{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

import           Prelude               hiding (getLine, putStrLn, readFile)

import           Control.Lens          (view, assign, ix, modifying, preview, use,
                                        uses, _3)
import           Control.Monad         (when)
import           Control.Monad.Loops   (whileM_)
import           Control.Monad.Reader  (MonadReader)
import           Control.Monad.RWS.CPS (RWST, evalRWST, execRWST, get, put,
                                        runRWST, tell)
import           Control.Monad.State   (MonadState)
import           Control.Monad.Writer  (MonadWriter)
import           Data.Array            (elems, listArray, (!), (//))
import           Data.Generics.Labels  ()
import           Data.List             (uncons, permutations)
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text, pack, splitOn, unpack)
import           Data.Text.IO          (getLine, putStrLn, readFile)
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty(..), fromList, toList)

import           Intcode
import           Internal

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
example_1 :: [Int]
example_1 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]

example_2 :: [Int]
example_2 = [3,23,3,24,1002,24,10,24,1002,23,-1,23,
             101,5,23,23,1,24,23,23,4,23,99,0,0]

run_amplifier :: [Int] -> Int -> Int -> Int
run_amplifier code input phase =
  let out = view _3 . runIdentity $
        launch (push_input input >> push_input phase >> run_program) code
  in read . unpack $ out

run_amplifier_chain :: [Int] -> Int -> NonEmpty Int -> Int
run_amplifier_chain code input (phase :| []) = run_amplifier code input phase
run_amplifier_chain code input (phase :| x : t) =
  let stage_output = run_amplifier code input phase
  in run_amplifier_chain code stage_output (x :| t)

perms :: NonEmpty a -> [NonEmpty a]
perms = fmap fromList <$> permutations . toList

find_solution_1 :: [Int] -> Int
find_solution_1 code =
  maximum (run_amplifier_chain code 0 <$> perms (0 :| [1..4]))

main :: IO ()
main = do
  code :: [Int] <- fmap (read . unpack) . splitOn "," <$> readFile "input.txt"

  putStrLn $  "Solution 1: " <> (pack . show $ find_solution_1 code)
