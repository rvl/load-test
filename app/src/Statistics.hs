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

data Statistics = Statistics { statsMeasurementCount :: Int }
  deriving (Show, Generic)

instance FromJSON Statistics
instance ToJSON Statistics
