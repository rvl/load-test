{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module API
  ( API
  , DeviceAPI
  ) where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Servant
import Servant.Server.Experimental.Auth

import qualified Database as DB
import Device
import Statistics

type DeviceAPI =
  "command" :> Get '[JSON] CommandAction
  :<|> "measurement" :> ReqBody '[JSON] Measurement :> Post '[JSON] NoContent

type UnprotectedAPI = "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginRequest
  :<|> "statistics" :> Get '[JSON] Statistics

type API = UnprotectedAPI :<|> AuthProtect "cookie-auth" :> DeviceAPI

-- | A value holding our type-level API
genAPI :: Proxy API
genAPI = Proxy

-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "cookie-auth") = DeviceId
