{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Perlude

import           Control.Lens         (_Left, over)
import           Data.Maybe           (mapMaybe)
import           Data.Time            (TimeOfDay (..), UTCTime (..),
                                       fromGregorian, timeOfDayToTime)
import           Text.Parsec          (noneOf, spaces)
import           Text.Parsec.Parselib (Parser, digitsAsNum, literal, parseAll,
                                       parsePart, text)

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
  <$> day <* literal " " <* weekday <* literal " "
  <*> time

day :: Parser (Integer, Int, Int)
day =
  (,,)
  <$> digitsAsNum <* literal "-"
  <*> digitsAsNum <* literal "-"
  <*> digitsAsNum

weekday :: Parser Text
weekday = text (noneOf " ")

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

parseArgs :: [Text] -> Either Text (Integer, Int, Int)
parseArgs = \case
  [arg] -> over _Left show  $ parseAll day arg
  _     -> Left "You must pas a cut date, like 2020-12-30"

getCutDate :: IO (Integer, Int, Int)
getCutDate = getArgs >>= either fail return . parseArgs

main :: IO ()
main = do
  cutDate <- getCutDate
  interact $ unlines . mapMaybe (filterLine cutDate) . lines
