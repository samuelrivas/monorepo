{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import           Prelude               hiding (getLine, putStrLn, readFile)

import           Control.Lens          (assign, use, uses, view, _2, _3)
import           Control.Monad.Loops   (whileM)
import           Control.Monad.State   (MonadState, evalState)
import           Data.Functor.Identity (runIdentity)
import           Data.Generics.Labels  ()
import           Data.List             (permutations)
import           Data.List.NonEmpty    (NonEmpty (..), fromList, toList)
import           Data.Text             (pack, splitOn, unpack)
import           Data.Text.IO          (putStrLn, readFile)
import           GHC.Generics          (Generic)

import           Intcode
import           Internal

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
example_1 :: [Int]
example_1 = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]

example_2 :: [Int]
example_2 = [1102,34915192,34915192,7,4,7,99,0]

example_3 :: [Int]
example_3 = [104,1125899906842624,99]

main :: IO ()
main = do
  code :: [Int] <- fmap (read . unpack) . splitOn "," <$> readFile "input.txt"
  undefined
