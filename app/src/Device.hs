{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Device
  ( DeviceId
  , Command(..)
  , CommandAction(..)
  , Measurement'(..)
  , Measurement
  , LoginRequest(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics
import Data.UUID

import Database

type DeviceId = UUID

data CommandAction = Stay | ChargeFromGrid | DischargeToGrid deriving (Show, Eq, Generic)

data LoginRequest = LoginRequest { loginRequestDeviceId :: Text }

instance FromJSON LoginRequest where
  parseJSON = withObject "Login Request" $
              \o -> LoginRequest <$> (o .: "username")

instance ToJSON LoginRequest where
  toJSON (LoginRequest deviceId) = object [ "username" .= deviceId ]

instance FromJSON CommandAction
instance ToJSON CommandAction

instance ToJSON Measurement where
  toJSON Measurement{..} = object
    [ "id" .= measurementDeviceId
    , "time" .= measurementTime
    , "charge" .= measurementCharge
    , "max_charge" .= measurementMaxCharge
    , "charge_rate" .= measurementChargeRate
    , "temp" .= measurementTemp ]

instance FromJSON Measurement where
  parseJSON = withObject "Measurement" $ \o ->
    Measurement <$>
    (o .:? "id".!= nil) <*>
    (o .: "time") <*>
    (o .: "charge") <*>
    (o .: "max_charge") <*>
    (o .: "charge_rate") <*>
    (o .: "temp")
