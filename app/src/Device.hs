{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Device
  ( DeviceId
  , Command'(..)
  , Command
  , CommandAction(..)
  , Measurement'(..)
  , Measurement
  , LoginRequest(..)
  , ChargeBattery(..)
  , FillReservoir(..)
  , FlushReservoir(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics
import Data.UUID

import Database

type DeviceId = UUID

data CommandAction = CommandAction ChargeBattery FillReservoir FlushReservoir
  deriving (Show, Eq, Generic)

data ChargeBattery = NoChargeBattery | ChargeBattery
  deriving (Show, Eq, Generic)
data FillReservoir = NoFillReservoir | FillReservoir
  deriving (Show, Eq, Generic)
data FlushReservoir = NoFlushReservoir | FlushReservoir Double
  deriving (Show, Eq, Generic)

data LoginRequest = LoginRequest { loginRequestDeviceId :: Text }

instance FromJSON LoginRequest where
  parseJSON = withObject "Login Request" $
              \o -> LoginRequest <$> (o .: "username")

instance ToJSON LoginRequest where
  toJSON (LoginRequest deviceId) = object [ "username" .= deviceId ]

instance FromJSON CommandAction where
  parseJSON = withObject "Command Action" $ \o ->
    CommandAction <$> o .: "charge" <*> o .: "fill" <*> o .: "flush"

instance ToJSON CommandAction where
  toJSON (CommandAction charge fill flush) = object
    [ "charge" .= charge
    , "fill"   .= fill
    , "flush"  .= flush ]

instance ToJSON FillReservoir
instance ToJSON FlushReservoir
instance ToJSON ChargeBattery
instance FromJSON FillReservoir
instance FromJSON FlushReservoir
instance FromJSON ChargeBattery

instance ToJSON Measurement where
  toJSON Measurement{..} = object
    [ "id"           .= measurementDeviceId
    , "time"         .= measurementTime
    , "charge_now"   .= measurementChargeNow
    , "charge_full"  .= measurementChargeFull
    , "charge_rate"  .= measurementChargeRate
    , "ambient_temp" .= measurementAmbientTemp
    , "water_temp"   .= measurementWaterTemp
    , "water_level"  .= measurementWaterLevel ]

instance FromJSON Measurement where
  parseJSON = withObject "Measurement" $ \o ->
    Measurement           <$>
    (o .:? "id".!= nil)   <*>
    (o .: "time")         <*>
    (o .: "charge_now")   <*>
    (o .: "charge_full")  <*>
    (o .: "charge_rate")  <*>
    (o .: "ambient_temp") <*>
    (o .: "water_temp")   <*>
    (o .: "water_level")
