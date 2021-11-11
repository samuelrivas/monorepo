{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Perlude

import           Data.Fixed
import           Data.Maybe
import           Data.Time
import           Data.Time.Clock
import           Text.Parsec
import           Text.Parsec.Parselib

example :: Text
example = "    CLOCK: [2021-10-13 Wed 07:51]--[2021-10-13 Wed 08:02] =>  0:11"


clockLine :: Parser (UTCTime, UTCTime)
clockLine = do
  spaces
  literal "CLOCK: ["
  start <- date
  literal "]--["
  stop <- date
  literal "] => "
  return (start, stop)

date :: Parser UTCTime
date =
  mkUTCTime
  <$> day <* literal " "
  <*> time

day :: Parser (Integer, Int, Int)
day =
  (,,)
  <$> digitsAsNum <* literal "-"
  <*> digitsAsNum <* literal "-"
  <*> digitsAsNum <* literal " " <* text (noneOf " ")

time :: Num n => Parser (n, n)
time =
  (,)
  <$> digitsAsNum <* literal ":"
  <*> digitsAsNum

-- TODO: move to library
mkUTCTime :: (Integer, Int, Int) -> (Int, Int) -> UTCTime
mkUTCTime (year, mon, day) (hour, min) =
  UTCTime (fromGregorian year mon day)
  (timeOfDayToTime (TimeOfDay hour min 0))

filterLine :: (Integer, Int, Int) -> Text -> Maybe Text
filterLine cutDate text =
  let
    cutUtc = mkUTCTime cutDate (0, 0)
  in
    case parsePart clockLine text of
      Right ((_startTime, endTime), _ ) | endTime < cutUtc -> Nothing
      _                                                    -> Just text

main :: IO ()
main = interact $ unlines . mapMaybe (filterLine (2021, 10, 10)) . lines
