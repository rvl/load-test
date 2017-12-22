{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TemplateHaskell    #-}

module Config
  ( Config(..)
  , ResourcePoolConfig(..)
  , ConnectInfo(..)
  , HostName
  , AccessLogLevel(..)
  , def
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
                     , configDatabaseConnectionPool   :: ResourcePoolConfig
                     }
  deriving(Read, Show, Generic)

data ResourcePoolConfig = ResourcePoolConfig
                          { configNumStripes :: Int
                          , configNumResources :: Int
                          }
  deriving(Read, Show, Generic)

instance FromJSON Config where parseJSON = genericParseJSON prefixOptions
instance FromJSON ResourcePoolConfig where parseJSON = genericParseJSON prefixOptions

-- | Default instance for testing convenience
instance Default Config where
  def = Config { configPort                     = 8080
               , configHostname                 = "localhost"
               , configAccessLogLevel           = Enabled
               , configDatabaseConnectionString = "postgresql:///load_test"
               , configDatabaseConnectionPool   = def
               }

instance Default ResourcePoolConfig where
  def = ResourcePoolConfig { configNumStripes   = 4
                           , configNumResources = 4
                           }
