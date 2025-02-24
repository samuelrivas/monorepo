-- {-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main (
  main,
  )
where

import           Perlude

import           Control.Monad (when)
import           Data.Text     (dropEnd)
import           HSH           (exit, run)
import           System.IO     (stderr)

data BatteryStatus = Unknown | Critical | Low | Normal | High | Full
  deriving stock (Show, Eq, Ord)

data Urgency = Alert | Regular
  deriving stock Show

urgency :: BatteryStatus -> Urgency
urgency Unknown  = Alert
urgency Critical = Alert
urgency _        = Regular

urgencyToText :: Urgency -> Text
urgencyToText Alert   = "critical"
urgencyToText Regular = "normal"

parseBatteryLevel :: Text -> Maybe BatteryStatus
parseBatteryLevel "Unknown"  = Just Unknown
parseBatteryLevel "Critical" = Just Critical
parseBatteryLevel "Low"      = Just Low
parseBatteryLevel "Normal"   = Just Normal
parseBatteryLevel "High"     = Just High
parseBatteryLevel "Full"     = Just Full
parseBatteryLevel _          = Nothing

warnLowBattery :: BatteryStatus -> IO ()
warnLowBattery s =
  let
    u = urgency s
  in
    run $ unpack $
    "notify-send -u " <> urgencyToText u <> " \"Battery " <> show s <>"\""

-- https://www.kernel.org/doc/Documentation/ABI/testing/sysfs-class-power
readSysFile :: MonadIO m => m Text
readSysFile = dropEnd 1 <$> readFile "/sys/class/power_supply/BAT1/capacity_level"

main :: IO ()
main = do
  maybeLevel <- parseBatteryLevel <$> readSysFile
  case maybeLevel of
    Nothing ->
      do
        hPutStrLn stderr "Could not parse battery level"
        exit  1
    Just level ->
      when (level < Normal) $ warnLowBattery level
