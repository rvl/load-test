{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TemplateHaskell    #-}

module Config
  ( Config(..)
  , ConnectInfo(..)
  , HostName
  , AccessLogLevel(..)
  ) where

import Data.Aeson
import Data.Aeson.Extra           (prefixOptions)
import Data.Text                  (Text)
import Database.PostgreSQL.Simple (ConnectInfo (..))
import GHC.Generics
import Network.Wai.Handler.Warp   (Port)
import Data.Default

type HostName = Text

-- | Access logging level
data AccessLogLevel = Disabled | Enabled | Development
  deriving(Read, Show, Generic)

instance FromJSON AccessLogLevel

data Config = Config { configPort                     :: Port
                     , configHostname                 :: HostName
                     , configAccessLogLevel           :: AccessLogLevel
                     , configDatabaseConnectionString :: Text
                     }
  deriving(Read, Show, Generic)

instance FromJSON Config where parseJSON = genericParseJSON prefixOptions

-- | Default instance for testing convenience
instance Default Config where
  def = Config { configPort                     = 8080
               , configHostname                 = "localhost"
               , configAccessLogLevel           = Enabled
               , configDatabaseConnectionString = "postgresql:///load_test"
               }
