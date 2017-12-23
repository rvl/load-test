{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  RecordWildCards #-}

module Statistics
  ( Statistics(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics

data Statistics = Statistics { statsMeasurementCount :: Int -- ^ number of measurements taken
                             , statsNumDevices :: Int -- ^ number of devices
                             , statsTotalBatteryCharge :: Int -- ^ charge of all devices together
                             , statsTotalWaterVolume :: Int -- ^ sum of all devices water level
                             , statsMeanTemperature :: [(Int, (Double, Double))] -- ^ by current hour, temp and std dev
                             , statsTotalWaterEnergy :: Double -- ^ potential energy of water in joules
                             }
  deriving (Show, Generic)

instance FromJSON Statistics
instance ToJSON Statistics
