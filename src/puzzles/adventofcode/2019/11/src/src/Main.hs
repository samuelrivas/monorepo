-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import           Prelude               hiding (getLine, putStrLn, readFile)

import           Control.Lens          (use, non, view, _3, at)
import           Data.Functor.Identity (runIdentity)
import           Data.Generics.Labels  ()
import           Data.Text             (Text, splitOn, unpack)
import           Data.Text.IO          (putStrLn, readFile)
import Control.Monad.State (State, lift)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)

import           Intcode

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

data Colour = Black | White
  deriving stock (Show, Eq, Enum)

data Move = Left | Right
  deriving stock (Show, Eq, Enum)

encode :: (Num c, Enum a) => a -> c
encode = fromIntegral . fromEnum

decode :: (Integral c, Enum a) => c -> a
decode = toEnum . fromIntegral

type Coord = (Int, Int)

data Panels = Panels {
  panels :: Map Coord Colour,
  pos :: Coord
  } deriving stock (Show, Generic)

type Robot = ProgramT (State Panels)


step :: Robot (Maybe (Move, Colour))
step = do
  location <- lift $ use #pos
  on_colour <- lift $ use (#panels . at location . non Black)
  push_input [encode on_colour]
  run_program
  get_output >>= \case
    [move, colour] -> do
      flush_output
      pure . Just $ (decode move, decode colour)
    _ -> do
      abort "Didn't get two outputs!"
      pure Nothing

main :: IO ()
main = do
  code :: [Integer] <- fmap (read . unpack) . splitOn "," <$> readFile "input.txt"
  undefined
