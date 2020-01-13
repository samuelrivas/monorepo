-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Advent.Day5 (main) where

import           Perlude

import           Control.Monad         (unless)

import           Control.Monad.Intcode

passDiagnose :: [Integer] -> Bool
passDiagnose = all (== 0) . init

diagnose :: Monad m => Integer -> IntcodeT m [Integer]
diagnose mode = pushInput [mode] >> runProgram >> getOutput

main :: IO ()
main = do
  code <- codeForDay "5"

  (out1, _) <- eval (diagnose 1) code
  unless (passDiagnose out1) $ error $ "Something failed :( " <> show out1
  putStrLn $ "Solution 1: " <> show (last out1)

  (out2, _) <- eval (diagnose 5) code
  putStrLn $  "Solution 2: " <> show (head out2)
