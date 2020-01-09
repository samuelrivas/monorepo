-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Main (
  main,
  test,
  test2
  )
where

import           Control.Applicative  ((<|>))
import           Control.Lens         (view)
import           Control.Monad        (unless)
import           Data.Char            (digitToInt)
import           Data.Foldable        (foldl')
import           Data.Functor         (($>))
import           Data.Generics.Labels ()
import           GHC.Generics         (Generic)
import           HSH                  (exit, run)
import           Text.Parsec          (Parsec, char, digit, many, many1, noneOf,
                                       parse, skipMany, space, spaces, string)

data AcpiStatus = AcpiStatus
  { timeLeft   :: (Int, Int, Int),
    isCharging :: Bool
  } deriving (Show, Generic)

test :: String
test = "Battery 0: Discharging, 63%, 05:46:02 remaining\n"

test2 :: String
test2 = "Battery 0: Charging, 9%, 01:48:25 until charged\n"

acpiCommand :: String
acpiCommand = "acpi -b"

acpiLine :: Parsec String st AcpiStatus
acpiLine = do
  plugged <- batteryStatus
  skipMany (space <|> comma)
  skipMany notComma
  skipMany (space <|> comma)
  t <- remaining
  pure $ AcpiStatus t plugged

batteryStatus :: Parsec String st Bool
batteryStatus =
  many notColon *> colon *> spaces *>
  (string "Discharging" $> False <|> string "Charging" $> True)

remaining :: Parsec String st (Int, Int, Int)
remaining = time <* spaces <* (string "remaining" <|> string "until charged")

time :: Parsec String st (Int, Int, Int)
time = (,,) <$> (number <* colon) <*> (number <* colon) <*> number

comma :: Parsec String st Char
comma = char ','

notComma :: Parsec String st Char
notComma = noneOf ","

colon :: Parsec String st Char
colon = char ':'

notColon :: Parsec String st Char
notColon = noneOf ":"

number :: Parsec String st Int
number = foldl' (\n c -> n * 10 + digitToInt c) 0 <$> many1 digit

toMinutes :: (Int, Int, Int) -> Int
toMinutes (h, m, _) = 60 * h + m

handleAcpiOutput :: String -> IO ()
handleAcpiOutput acpiOutput =
  case parse acpiLine acpiOutput acpiOutput of
    Right status ->
      let minutesLeft = toMinutes $ view #timeLeft status
      in unless (view #isCharging status || minutesLeft > warningMinutes) $
         warnLowBattery minutesLeft
    Left err -> do
      print err
      exit 1

timeMessage :: Int -> String
timeMessage t = show t <> " minutes left"

criticalMinutes :: Int
criticalMinutes = 30

warningMinutes :: Int
warningMinutes = 40

urgency :: Int -> String
urgency x
  | x < criticalMinutes = "critical"
  | otherwise = "normal"

warnLowBattery :: Int -> IO ()
warnLowBattery t =
  run $
  "notify-send -u " <> urgency t <> " \"Battery low\" " <> show (timeMessage t)

main :: IO ()
main = run acpiCommand >>= handleAcpiOutput
