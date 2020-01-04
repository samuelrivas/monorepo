-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts #-}

module Main (
  main,
  test
  ) where

import           Control.Monad (when)
import           Data.Char     (digitToInt)
import           Data.Foldable (foldl')
import           HSH           (exit, run)
import           Text.Parsec

test :: String
test = "Battery 0: Discharging, 63%, 05:46:02 remaining\n"

acpiCommand :: String
acpiCommand = "acpi -b"

comma :: Parsec String st Char
comma = char ',' <* spaces

notComma :: Parsec String st Char
notComma = noneOf ","

colon :: Parsec String st Char
colon = char ':'

number :: Parsec String st Int
number = foldl' (\n c -> n * 10 + digitToInt c) 0 <$> many1 digit

remaining :: Parsec String st (Int, Int, Int)
remaining = time <* spaces <* string "remaining"

timeLeft :: Parsec String st (Int, Int, Int)
timeLeft = many notComma *> comma *> many notComma *> comma *> remaining

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
      when (minutes < 10) $ warnLowBattery minutes
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
