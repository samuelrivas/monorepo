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

import           Control.Lens          (assign, at, modifying, set, use, view)
import           Control.Monad         (when)
import           Control.Monad.Loops   (untilJust)
import           Control.Monad.RWS.CPS (RWST, evalRWST, execRWST, lift, tell)
import           Data.Foldable         (fold)
import           Data.Functor.Identity (runIdentity)
import           Data.Generics.Labels  ()
import           Data.Map.Strict       (Map, empty)
import           Data.Maybe            (isNothing)
import           Data.Sequence         (Seq ((:<|)), fromList, (><), (|>))
import qualified Data.Sequence         as Seq
import           Data.Text             (Text, pack, splitOn, unpack)
import           Data.Text.IO          (putStrLn, readFile)

import           Bidim
import           Intcode

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

main :: IO ()
main = do
  code <- getInput

  putStrLn $ "Solution 1: "
  putStrLn $ "Solution 2: "
