-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}

module Main (
  main,
  test,
  test2
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad       (when)
import           Data.Char           (digitToInt)
import           Data.Foldable       (foldl')
import           HSH                 (exit, run)
import           Text.Parsec         (Parsec, char, digit, many, many1, noneOf,
                                      parse, spaces, string, try)

data Status = Charging | Discharging Int
  deriving Show

test :: String
test = "Battery 0: Discharging, 63%, 05:46:02 remaining\n"

test2 :: String
test2 = "Battery 0: Charging, 9%, 01:48:25 until charged\n"

acpiCommand :: String
acpiCommand = "acpi -b"

comma :: Parsec String st Char
comma = char ',' <* spaces

notComma :: Parsec String st Char
notComma = noneOf ","

colon :: Parsec String st Char
colon = char ':'

notColon :: Parsec String st Char
notColon = noneOf ":"

number :: Parsec String st Int
number = foldl' (\n c -> n * 10 + digitToInt c) 0 <$> many1 digit

remaining :: Parsec String st (Int, Int, Int)
remaining = time <* spaces <* (string "remaining" <|> string "until charged")

charging :: Parsec String st ()
charging = spaces <* string "Charging"

discharging :: Parsec String st ()
discharging = spaces <* string "Discharging"

timeLeft :: Parsec String st (Int, Int, Int)
timeLeft = many notColon *> colon *>
           (try charging <|> try discharging) *>
           many notComma *> comma *>
           many notComma *> comma *> remaining

time :: Parsec String st (Int, Int, Int)
time = (,,) <$> (number <* colon) <*> (number <* colon) <*> number

minutesLeft :: Parsec String st Int
minutesLeft = toMinutes <$> timeLeft

toMinutes :: (Int, Int, Int) -> Int
toMinutes (h, m, _) = 60 * h + m

handleAcpiOutput :: String -> IO ()
handleAcpiOutput acpiOutput =
  case parse minutesLeft acpiOutput acpiOutput of
    Right minutes ->
      when (minutes < 20) $ warnLowBattery minutes
    Left err -> do
      print err
      exit 1

timeMessage :: Int -> String
timeMessage t = show t <> " minutes left"

urgency :: Int -> String
urgency x
  | x < 2 = "critical"
  | otherwise = "normal"

warnLowBattery :: Int -> IO ()
warnLowBattery t =
  run $ "notify-send -u " <>
        urgency t <>
        " \"Battery low\" " <>
        show (timeMessage t)

main :: IO ()
main = run acpiCommand >>= handleAcpiOutput
