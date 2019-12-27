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

import           Prelude                   hiding (Left, Right, concat, getLine,
                                            putStr, putStrLn, readFile, show)
import qualified Prelude

import           Control.Applicative       ((<|>))
import           Control.Lens              (assign, at, both, ix, modifying,
                                            productOf, set, use, uses, view,
                                            views, _1, _2, _Just)
import           Control.Monad             (when)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Loops       (untilJust, whileM)
import           Control.Monad.RWS.CPS     (RWST, evalRWST, execRWST, lift,
                                            tell)
import           Control.Monad.State
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Foldable             (fold, foldl')
import           Data.Functor.Identity     (runIdentity)
import           Data.Generics.Labels      ()
import           Data.HashMap.Strict       (HashMap, (!))
import qualified Data.HashMap.Strict       as HashMap
import           Data.List                 (unfoldr)
import           Data.List                 (find, maximumBy, sort, tails)
import           Data.Map.Strict           (Map, empty, insert, keys, toList)
import           Data.Maybe                (fromJust, fromMaybe)
import           Data.Monoid               (Sum (..))
import           Data.Sequence             (Seq ((:<|)), fromList, (><), (|>))
import qualified Data.Sequence             as Seq
import           Data.Text                 (Text, pack, splitOn, unpack)
import           Data.Text.IO              (putStr, putStrLn, readFile)
import           System.Console.Readline   (readline)

import           Intcode
import           Internal

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

allNodes :: [Integer]
allNodes = [0..49]

solution1 :: Monad m => [Integer] -> m Integer
solution1 intcode =
  let networkState = initialNetworkState intcode
  in view _2 <$> evalStateT runUntilNat networkState

initialNetworkState :: [Integer] -> NetworkState
initialNetworkState intcode =
  let
    intcodeState addr = view _2 . runIdentity $ run (initComputer addr) intcode
    nodes = (\addr -> (addr, mkNodeState . intcodeState $ addr)) <$> allNodes
  in
    NetworkState (HashMap.fromList nodes) Nothing Nothing True [] mempty

computerStep ::
  Monad m => Maybe [Integer] -> IntcodeT m ([Integer], Status, Bool)
computerStep inputM = do
  runProgram
  output <- getOutput
  status <- getStatus

  flushOutput

  when (status == Interrupted) $
    pushInput (fromMaybe [-1] inputM)

  input <- use #input -- We are breaking encapsulation here, but whatever..

  pure (output, status, input == [-1])

-- Returns packages sent
runNode :: Monad m => Integer -> NetworkT m ()
runNode addr = do
  nodeState <- uses #nodes (! addr)
  let computerState = view #computerState nodeState
      input = view #input nodeState

  ((output, _status, noInput), newState, _w) <-
    runEmpty (reset computerState >> computerStep input)

  when (_status /= Interrupted) $ do
    error "unexpected status"
    pure ()

  let newNode = set #computerState newState $
                set #input Nothing nodeState

  assign (#nodes . at addr) $ Just newNode

  unless (null output && noInput) $ do
    assign #idleCount (Sum 0)
    assign #idle False

  dispatchOutput output

dispatchOutput :: Monad m =>  [Integer] -> NetworkT m ()
dispatchOutput [] = pure ()
dispatchOutput (255 : x : y : t) = do
  assign #nat $ Just (x, y)
  dispatchOutput t
dispatchOutput (addr : x : y : t) = do
  node <- uses #nodes (! addr)
  let inputs :: [Integer] = view (#input . _Just) node
      newNode = set #input (Just $ x : y : inputs) node
  assign (#nodes . at addr) $ Just newNode
  dispatchOutput t
dispatchOutput other = error $ "weird output: " ++ Prelude.show other

networkStep :: Monad m => NetworkT m ()
networkStep = do
  assign #idle True
  sequence_ $ runNode <$> [0..49]
  natCheck

natCheck :: Monad m => NetworkT m ()
natCheck = do
  isIdle <- use #idle
  when isIdle $ modifying #idleCount (Sum 1 <>)

  count <- use #idleCount
  when (getSum count > 3) $
    use #nat >>= \case
      Just (x, y) -> do
        assign #lastNat (Just (x, y))
        assign #idleCount mempty
        dispatchOutput [0, x, y]
        sniffNat y
      Nothing -> error "nonat"

sniffNat :: Monad m => Integer -> NetworkT m ()
sniffNat y = do
  dups <- use #dupNats
  use #dupNats >>= \case
    [] -> assign #dupNats [y]
    [x] | x == y -> assign #dupNats [y, y]
        | otherwise -> assign #dupNats [y]
    _ -> error $ "sniffed too much: " <> Prelude.show dups
  -- modifying #dupNats (y :)

monitorNat :: Monad m => NetworkT m (Maybe (Integer, Integer))
monitorNat = do
  lastNat <- use #lastNat
  assign #lastNat Nothing
  pure lastNat

initComputer :: Monad m => Integer -> IntcodeT m ()
initComputer = pushInput . pure

runUntilNat :: Monad m => NetworkT m (Integer, Integer)
runUntilNat = untilJust (networkStep >> use #nat)

main :: IO ()
main = do
  code <- getInput

  sol1 <- solution1 code
  putStrLn $ "Solution 1: " <> show sol1
  putStrLn $ "Solution 2: "


-- Testing
initialisedIntcode :: Integer -> IO IntcodeState
initialisedIntcode addr = view _2 <$> (getInput >>= run (initComputer addr))

debugNetworkState :: IO NetworkState
debugNetworkState = do
  intcodeState <- initialisedIntcode 1
  let nodeState = mkNodeState intcodeState
  pure $ NetworkState (HashMap.singleton 1 nodeState) Nothing Nothing True [] mempty

mkNodeState :: IntcodeState -> NodeState
mkNodeState intcodeState = NodeState intcodeState Nothing
