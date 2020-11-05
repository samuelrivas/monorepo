{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day1 where

import           Data.Text.IO     (putStrLn)
import           Prelude          hiding (putStrLn)

import           System.IO.Advent (getInput)

main :: IO ()
main = getInput "1" >>= putStrLn
