{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Perlude

import           Data.Fixed
import           Data.Time
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


main :: IO ()
main = putStrLn "hi!"
