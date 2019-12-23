{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import           Prelude                 hiding (Left, Right, concat, getLine,
                                          putStr, putStrLn, readFile, show)
import qualified Prelude

import           Control.Applicative     ((<|>))
import           Control.Lens            (uses, assign, at, both, ix, modifying,
                                          productOf, set, use, view, views, _1,
                                          _2, _Just)
import Data.List (unfoldr)
import           Control.Monad           (when)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Loops     (untilJust)
import           Control.Monad.RWS.CPS   (RWST, evalRWST, execRWST, lift, tell)
import Control.Monad.State -- close this
import           Data.Foldable           (fold, foldl')
import           Data.Functor.Identity   (runIdentity)
import           Data.Generics.Labels    ()
import           Data.List               (find, maximumBy, sort, tails)
import           Data.Map.Strict         (Map, empty, insert, keys, toList)
import           Data.Maybe              (fromJust, fromMaybe)
import           Data.Sequence           (Seq ((:<|)), fromList, (><), (|>))
import qualified Data.Sequence           as Seq
import           Data.Text               (Text, pack, splitOn, unpack)
import           Data.Text.IO            (putStr, putStrLn, readFile)
import           System.Console.Readline (readline)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap

import           Intcode
import Internal

type NetworkT = StateT NetworkState

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

solution1 :: [Integer] -> Int
solution1 = undefined

computerStep ::
  Monad m => Maybe [Integer] -> IntcodeT m ([Integer], Status)
computerStep inputM = do
  runProgram
  output <- getOutput
  status <- getStatus

  flushOutput

  when (status == Interrupted) $
    pushInput (fromMaybe [-1] inputM)

  pure (output, status)

-- Returns packages sent
runNode :: Monad m => Integer -> NetworkT m [Integer]
runNode addr = do
  nodeState <- uses #nodes (! addr)
  let computerState = view #computerState nodeState
      input = view #input nodeState

  ((output, _status), newState, _w) <-
    runEmpty (reset computerState >> computerStep input)

  let newNode = set #computerState newState $
                set #input Nothing nodeState

  assign (#nodes . at addr) $ Just newNode

  dispatchOutput output
  pure output

dispatchOutput :: Monad m =>  [Integer] -> NetworkT m ()
dispatchOutput [] = pure ()
dispatchOutput (addr : x : y : t) = do
  node <- uses #nodes (! addr)
  let inputs :: [Integer] = view (#input . _Just) node
      newNode = set #input (Just $ x : y : inputs) node
  assign (#nodes . at addr) $ Just newNode
  dispatchOutput t
dispatchOutput other = error $ "weird output: " ++ Prelude.show other

networkStep :: Monad m => NetworkT m [[Integer]]
networkStep = sequence $ runNode <$> [0..49]

initComputer :: Monad m => Integer -> IntcodeT m ()
initComputer = pushInput . pure

main :: IO ()
main = do
  code <- getInput

  putStrLn $ "Solution 1: " <> show (solution1 code)
  putStrLn $ "Solution 2: "


-- Testing
initialisedIntcode :: Integer -> IO IntcodeState
initialisedIntcode id = view _2 <$> (getInput >>= run (initComputer id))

debugNetworkState :: IO NetworkState
debugNetworkState = do
  intcodeState <- initialisedIntcode 1
  let nodeState = mkNodeState intcodeState
  pure . NetworkState $ HashMap.singleton 1 nodeState

mkNodeState :: IntcodeState -> NodeState
mkNodeState intcodeState = NodeState intcodeState Nothing

initialNetworkState :: [Integer] -> NetworkState
initialNetworkState intcode =
  let
    intcodeState addr = view _2 . runIdentity $ run (initComputer addr) intcode
    nodes = (\addr -> (addr, mkNodeState . intcodeState $ addr)) <$> [0..49]
  in
    NetworkState $ HashMap.fromList nodes
    
