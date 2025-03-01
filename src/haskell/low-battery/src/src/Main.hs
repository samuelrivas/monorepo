{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

-- Status and capacity levels are documented here:
-- https://www.kernel.org/doc/Documentation/ABI/testing/sysfs-class-power

data CapacityLevel = ClUnknown | Critical | Low | Normal | High | ClFull
  deriving stock (Show, Eq, Ord)

data Status = StUnknown | Charging | Discharging | NotCharging | StFull
  deriving stock (Show, Eq)

data Urgency = Alert | Regular
  deriving stock Show

urgency :: CapacityLevel -> Urgency
urgency ClUnknown = Alert
urgency Critical  = Alert
urgency _         = Regular

urgencyToText :: Urgency -> Text
urgencyToText Alert   = "critical"
urgencyToText Regular = "normal"

parseBatteryLevel :: MonadFail m => Text -> m CapacityLevel
parseBatteryLevel "Unknown"  = pure ClUnknown
parseBatteryLevel "Critical" = pure Critical
parseBatteryLevel "Low"      = pure Low
parseBatteryLevel "Normal"   = pure Normal
parseBatteryLevel "High"     = pure High
parseBatteryLevel "Full"     = pure ClFull
parseBatteryLevel other      = fail $ "cannot parse battery level: " <> other

parseStatus :: MonadFail m => Text -> m Status
parseStatus "Unknown"      = pure StUnknown
parseStatus "Charging"     = pure Charging
parseStatus "Discharging"  = pure Discharging
parseStatus "Not charging" = pure NotCharging
parseStatus "Full"         = pure StFull
parseStatus other          = fail $ "cannot parse status: " <> other

unplugged :: Status -> Bool
unplugged status = status == Discharging || status == StUnknown

warnLowBattery :: CapacityLevel -> IO ()
warnLowBattery s =
  let
    u = urgency s
  in
    run $ unpack $
    "notify-send -u " <> urgencyToText u <> " \"Battery " <> show s <>"\""

-- Just drop the final new line
readSysFile :: MonadIO m => FilePath -> m Text
readSysFile = fmap (dropEnd 1) . readFile

getCapacityLevel :: MonadIO m => MonadFail m => m CapacityLevel
getCapacityLevel =
  readSysFile "/sys/class/power_supply/BAT1/capacity_level"
  >>= parseBatteryLevel

getStatus :: MonadIO m => MonadFail m => m Status
getStatus =
  readSysFile "/sys/class/power_supply/BAT1/status"
  >>= parseStatus

info :: MonadIO m => Text -> m ()
info = hPutStrLn stderr

main :: IO ()
main =
  do
    info "Reading battery status"
    status <- getStatus
    capacityLevel <- getCapacityLevel
    info $ "Battery status: " <> show status
    info $ "Battery capacity level: " <> show capacityLevel
    when (capacityLevel < Normal && unplugged status ) $ warnLowBattery capacityLevel
