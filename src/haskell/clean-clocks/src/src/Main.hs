{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Perlude

import           Control.Applicative  ((<|>))
import           Control.Lens         (_2, _3, _Left, over, view)
import           Data.Functor         (($>))
import           Data.Maybe           (mapMaybe)
import           Data.Time            (TimeOfDay (..), UTCTime (..),
                                       fromGregorian, timeOfDayToTime)
import           Text.Parsec          (between, noneOf, spaces, try)
import           Text.Parsec.Parselib (Parser, digitsAsNum, literal, parseAll,
                                       parsePart, text)

exampleClockLine :: Text
exampleClockLine =
  "    CLOCK: [2021-10-13 Wed 07:51]--[2021-10-13 Wed 08:02] =>  0:11"

exampleTransitionLine :: Text
exampleTransitionLine =
  "    - State \"DONE\"       from \"TODO\"       [2021-11-13 Sat 21:02]"

lineWithDate :: Parser UTCTime
lineWithDate =
  (view _2 <$> try clockLine)
  <|> view _3 <$> try stateTransition

clockLine :: Parser (UTCTime, UTCTime)
clockLine =
  (,)
  <$> (spaces *> literal "CLOCK: [" *> date)
  <*> (literal "]--[" *> date <*  literal "] => ")

quote :: Parser ()
quote = literal "\"" $> ()

quotedText :: Parser Text
quotedText = between quote quote (text $ noneOf "\"")

stateTransition :: Parser (Text, Text, UTCTime)
stateTransition =
  (,,)
  <$> (spaces *> literal "- State " *> quotedText)
  <*> (spaces *> literal "from " *> quotedText)
  <*> (spaces *> between (literal "[") (literal "]") date)

reschedule = undefined

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
mkUTCTime (year, mon, day) (hour, minute) =
  UTCTime (fromGregorian year mon day)
  (timeOfDayToTime (TimeOfDay hour minute 0))

filterLine :: (Integer, Int, Int) -> Text -> Maybe Text
filterLine cutDate text =
  let
    cutUtc = mkUTCTime cutDate (0, 0)
  in
    case parsePart lineWithDate text of
      Right (endTime, _ ) | endTime < cutUtc -> Nothing
      _                                      -> Just text

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
