-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

import           Prelude               hiding (Left, Right, concat, getLine,
                                        putStrLn, readFile, show)

import           Control.Lens          (assign, at, modifying, non, over, set,
                                        toListOf, traverse, use, view, _1, _2,
                                        _3)
import           Control.Monad.Loops   (whileM_)
import           Control.Monad.State   (StateT, lift, runStateT)
import           Data.Foldable         (maximum, minimum)
import           Data.Functor.Identity (runIdentity)
import           Data.Generics.Labels  ()
import           Data.Map.Strict       (Map, empty, keys, size)
import           Data.Text             (Text, concat, intercalate, splitOn,
                                        unpack)
import           Data.Text.IO          (putStrLn, readFile)
import           GHC.Generics          (Generic)

import Internal

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

type Coord = (Integer, Integer, Integer)

sum_coord :: Coord -> Coord -> Coord
sum_coord (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

main :: IO ()
main = undefined

