{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Advent.Day22 where

import           Advent.Perlude

import           Control.Lens              (at, both, each, foldlOf, modifying,
                                            non, over, preuse, use, uses, view,
                                            _1, _2, _head)
import           Control.Monad             (guard)
import           Control.Monad.Loops       (iterateUntil, untilJust)
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Generics.Labels      ()
import           Data.HashSet              (HashSet)
import qualified Data.HashSet              as HashSet
import           Data.List                 (find, foldl', sort, unfoldr)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromJust, isJust)
import qualified Data.Text                 as Text
import qualified System.IO.Advent          as IOAdvent

import           Advent.Day22.Internal

-- TODO: put this somewhere (or import one of the many libs that define it
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM condM a b = do
  cond <- condM
  if cond then a else b

example :: Text
example =
  "Player 1:\n\
  \9\n\
  \2\n\
  \6\n\
  \3\n\
  \1\n\
  \\n\
  \Player 2:\n\
  \5\n\
  \8\n\
  \4\n\
  \7\n\
  \10\n"

getInput :: IO Text
getInput = IOAdvent.getInput "22"

parse :: Text -> Game
parse text =
  let (p1, p2) = over both Text.strip $ Text.breakOn "\n\n" text
  in mkGame (parsePlayer p1) (parsePlayer p2)

parsePlayer :: Text -> [Int]
parsePlayer = fmap read . tail . Text.lines

-- Returns true if the game ends
-- TODO: This may be more elegant with MaybeT
step :: MonadState Game m => m Bool
step = do
  h1M <- preuse $ #deck1 . _head
  h2M <- preuse $ #deck2 . _head
  case (,) <$> h1M <*> h2M of
    Nothing -> pure True
    Just (h1, h2)
      | h1 > h2 -> do
          modifying #deck1 $ tail . (++ [h1, h2])
          modifying #deck2 tail
          pure False
      | otherwise -> do
          modifying #deck1 tail
          modifying #deck2 $ tail . (++ [h2, h1])
          pure False

runGame :: MonadState Game m => m ([Int], [Int])
runGame = do
  _ <- iterateUntil id step
  (,) <$> use #deck1 <*> use #deck2

isCycle :: MonadState Game m => m Bool
isCycle = do
  d1 <- use #deck1
  d2 <- use #deck2
  uses #rounds $ HashSet.member (d1, d2)

recursiveStep :: MonadIO m => MonadState Game m => m (Maybe Bool)
recursiveStep =
  ifM isCycle (pure $ Just True) $ do
    d1 <- use #deck1
    d2 <- use #deck2

    -- putStrLn "Current state:\n"
    -- print d1
    -- print d2

    modifying #rounds $ HashSet.insert (d1, d2)
    case (d1, d2) of
      ([], _) -> pure $ Just False
      (_, []) -> pure $ Just True
      (h1:t1, h2:t2) -> do
        p1Winner <-
          -- TODO: I got this wrong by putting < instead of <=. But I failed to
          -- debug it. Practice a bit more with the debugger here

          if (h1 <= length t1) && (h2 <= length t2)
          then do

            -- putStrLn "recursing with:\n"
            -- putStrLn "===============\n"
            -- print (take h1 t1)
            -- print (take h2 t2)

            playRecursiveGame (take h1 t1) (take h2 t2)
          else pure $ h1 > h2

        if p1Winner
          then do
              modifying #deck1 $ tail . (++ [h1, h2])
              modifying #deck2 tail
              pure Nothing
          else do
              modifying #deck1 tail
              modifying #deck2 $ tail . (++ [h2, h1])
              pure Nothing

playRecursiveGame :: MonadIO m => [Int] -> [Int] -> m Bool
playRecursiveGame d1 d2 = evalStateT runRecursiveGame $ mkGame d1 d2

runRecursiveGame :: MonadIO m => MonadState Game m => m Bool
runRecursiveGame =  untilJust recursiveStep

solution1 :: Text -> Int
solution1 text =
  let
    initialState = parse text
    (p1, p2) = evalState runGame initialState
    winnerDeck = if null p1 then p2 else p1
  in
    sum . zipWith (*) [1..] $ reverse winnerDeck

solution2 :: Text -> IO Int
solution2 text = do
  (winner, st) <- runStateT runRecursiveGame (parse text)
  let
    winnerDeck = if winner then #deck1 else #deck2

  pure $ sum . zipWith (*) [1..] $ reverse (view winnerDeck st)

main :: IO ()
main = do
  input <- getInput

  putStr "Solution 1: "
  print . solution1 $ input

  putStr "Solution 2: "
  sol2 <- solution2 input
  print sol2
