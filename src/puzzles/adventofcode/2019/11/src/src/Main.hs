{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import Control.Monad.State (State, StateT, lift, runStateT)
import Data.Map.Strict (Map, empty)
import GHC.Generics (Generic)

import           Intcode
import           Internal

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

type RobotT m = ProgramT (StateT Panels m)

initial_panels :: Panels
initial_panels = Panels empty (0, 0)

step :: Monad m => RobotT m (Maybe (Move, Colour))
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

get_input :: IO [Integer]
get_input = fmap (read . unpack) . splitOn "," <$> readFile "input.txt"

run_robot ::
  Monad m => RobotT m a -> Panels -> [Integer] -> m (a, Status, Panels)
run_robot x initial_panels code = do
  ((a, comp_state, _w), final_panels) <- runStateT (launch x code) initial_panels
  pure (a, view #status comp_state, final_panels)

main :: IO ()
main = do
  code <- get_input
  undefined
