{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Device
  ( DeviceId
  , Command(..)
  , Measurement(..)
  , LoginRequest(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics
import Data.UUID

type DeviceId = UUID

data Command = Stay | ChargeFromGrid | DischargeToGrid deriving (Show, Eq, Generic)

data Measurement = Measurement
                   { mDeviceId :: DeviceId
                   , mTime :: POSIXTime
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
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Measurement
