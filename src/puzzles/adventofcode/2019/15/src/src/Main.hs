{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

import           Prelude                hiding (Left, Right, concat, getLine,
                                         putStrLn, readFile, show)

import           Control.Lens           (assign, at, ix, modifying, non, over,
                                         set, set, toListOf, traverse, use,
                                         uses, view, view, _1, _2, _3)
import Control.Monad.Trans.Maybe (runMaybeT)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Loops    (whileM_)
import           Control.Monad.State    (StateT, get, lift, runStateT, when)
import           Data.Foldable          (maximum, minimum, traverse_)
import           Data.Functor.Identity  (runIdentity)
import           Data.Generics.Labels   ()
import           Data.Map.Strict        (Map, empty, fromList, keys, size)
import           Data.Text              (Text, concat, intercalate, splitOn,
                                         unpack)
import           Data.Text.IO           (putStrLn, readFile)
import           GHC.Generics           (Generic)
import           System.Console.ANSI    (clearScreen, setCursorPosition)
import Data.Sequence (Seq, (<|), (|>), (><))

import           Bidim
import           Intcode                hiding (initial_state)
import           Internal

type ExploreT m = StateT Exploration m

encode :: (Num c, Enum a) => a -> c
encode = fromIntegral . fromEnum

decode :: (Integral c, Enum a) => c -> a
decode = toEnum . fromIntegral

encodeMove :: Move -> Integer
encodeMove = (+1) . encode

allMoves :: [Move]
allMoves = [minBound..maxBound]

getInput :: IO [Integer]
getInput = fmap (read . unpack) . splitOn "," <$> readFile "input.txt"

popNode :: Monad m => ExploreT m (Maybe Node)
popNode = undefined

moveDroid :: Monad m => Move -> IntcodeT m Cell
moveDroid = undefined

-- Returns the target node if found
expand :: Monad m => ExploreT m (Maybe Node)
expand = popNode >>= \case
  Just node -> expandNode node
  Nothing -> pure Nothing

expandNode :: Monad m => Node -> ExploreT m (Maybe Node)
expandNode node =
  let
    intcodeState = view #intcodeState node
    move = view #move node
    moveInstruction = reset intcodeState >> moveDroid move >> getOutput
  in do
    (intCell, intcodeState', _) <- lift $ runEmpty moveInstruction
    case decode <$> intCell of
      [Goal] -> pure $ Just node
      [Wall] -> undefined
      [Empty] -> undefined
      _ -> error "cannot decode!"

main :: IO ()
main = do
  code <- getInput
--  putStrLn $ "Solution 1: " <> show (size . fromList . parse_output . read_game $ code)
  undefined
