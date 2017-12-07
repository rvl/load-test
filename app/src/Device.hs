{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  RecordWildCards #-}

module Device
  ( DeviceId
  , Command(..)
  , Measurement(..)
  , LoginRequest(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics
import Data.UUID

type DeviceId = UUID

data Command = Stay | ChargeFromGrid | DischargeToGrid deriving (Show, Eq, Generic)

data Measurement = Measurement
                   { mDeviceId :: DeviceId
                   , mTime :: UTCTime
                   , mCharge :: Int
                   , mMaxCharge :: Int
                   , mChargeRate :: Double
                   , mTemp :: Double
                   } deriving (Show, Generic)

data LoginRequest = LoginRequest { loginRequestDeviceId :: Text }

instance FromJSON LoginRequest where
  parseJSON = withObject "Login Request" $
              \o -> LoginRequest <$> (o .: "username")

instance FromJSON Command
instance ToJSON Command

instance ToJSON Measurement where
  toJSON Measurement{..} = object
    [ "id" .= mDeviceId
    , "time" .= mTime
    , "charge" .= mCharge
    , "max_charge" .= mMaxCharge
    , "charge_rate" .= mChargeRate
    , "temp" .= mTemp ]

instance FromJSON Measurement where
  parseJSON = withObject "Measurement" $ \o ->
    Measurement <$>
    (o .:? "id".!= nil) <*>
    (o .: "time") <*>
    (o .: "charge") <*>
    (o .: "max_charge") <*>
    (o .: "charge_rate") <*>
    (o .: "temp")
