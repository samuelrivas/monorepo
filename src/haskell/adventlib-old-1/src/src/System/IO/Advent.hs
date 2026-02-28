{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module System.IO.Advent (
  getParsedInput,
  adventInputDir,
  getInput,
  adventPath,
  solve
  ) where

import           Perlude              hiding (getEnv)

import           Data.Advent          (Day (..))
import           System.FilePath      ((</>))
import           Text.Parsec.Parselib (Parser, unsafeParseAll)
import           UnliftIO.Environment (getEnv)

-- | Given a suitable 'Parser', get the parsed input for a given 'Day'
getParsedInput :: MonadIO m => MonadFail m => Day -> Parser a -> m a
getParsedInput d p = getInput d >>= unsafeParseAll p

adventInputDir :: MonadIO m => m FilePath
adventInputDir = getEnv "ADVENT_INPUT_DIR"

getInput :: MonadIO m => Day -> m Text
getInput day =
  let file = dayToString day <> ".txt"
  in (</> (unpack file)) <$> adventInputDir >>= liftIO . readFile

adventPath :: String -> FilePath -> IO FilePath
adventPath day path =  (</> day </> path) <$> adventInputDir

dayToString :: Day -> Text
dayToString = show . (+ 1) . fromEnum

solve :: MonadFail m
      => MonadIO m
      => Show b
      => Show c
      => Day -> Parser a -> (a -> b) -> (a -> c) -> m ()
solve day parser solver1 solver2 = do
  input <- getParsedInput day parser

  putStr "Solution 1: "
  print . solver1 $ input

  putStr "Solution 2: "
  print . solver2 $ input
