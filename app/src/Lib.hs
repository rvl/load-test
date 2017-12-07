{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}
module Lib
  ( startApp
  , server
  ) where

import Control.Monad                        (join)
import Control.Monad.Except
import Control.Monad.Log
import Control.Natural
import Data.Bifunctor                       (second)
import Data.Foldable                        (toList)
import Data.List                            (sortOn)
import Data.Maybe                           (catMaybes)
import Data.Monoid                          ((<>))
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (ByteString)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Safe                                 (headMay)
import Servant
import Servant.Utils.Enter                  (enter)
import Servant.Server.Experimental.Auth
import qualified Data.UUID as UUID

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO       as T

import API
import Config
import qualified Database as DB
-- import Query
import ServerEnv
import Device

startApp :: Config -> IO ()
startApp config = do
  let port = configPort config
      logging = loggingMiddleware config
  newEnv config >>= \case
    Nothing -> pure ()
    Just env -> do
      T.putStrLn $ "Serving on http://" <> configHostname config
                   <> ":" <> (pack . show $ port)
      run port . logging =<< app env

loggingMiddleware :: Config -> Middleware
loggingMiddleware config = case configAccessLogLevel config of
  Disabled    -> id
  Enabled     -> logStdout
  Development -> logStdoutDev

app :: Env -> IO Application
app env = pure $ serveWithContext api (appContext env) (server env)
  where  api = Proxy :: Proxy DeviceAPI

lookupAccount :: ByteString -> Servant.Handler DeviceId
lookupAccount i = case UUID.fromText (decodeUtf8 i) of
  Just uuid -> pure uuid
  Nothing -> throwError (err403 { errBody = "Invalid Cookie" })


authHandler :: AuthHandler Request DeviceId
authHandler =
  let handler req = case lookup "servant-auth-cookie" (requestHeaders req) of
        Nothing -> throwError (err401 { errBody = "Missing auth header" })
        Just authCookieKey -> lookupAccount authCookieKey
  in mkAuthHandler handler

appContext :: Env -> Context '[AuthHandler Request DeviceId]
appContext env = authHandler :. EmptyContext

appToHandler' :: forall a. Env -> App a -> Servant.Handler a
appToHandler' env r = do
  res <- liftIO $ runExceptT (runApp env r)
  case res of
    Left err -> throwError err
    Right a -> return a

appToHandler :: Env -> App :~> Servant.Handler
appToHandler env = NT (appToHandler' env)

server :: Env -> Server DeviceAPI
server env = enter (appToHandler env) api
  where api = handleCommand undefined :<|> handleMeasurement undefined

handleCommand :: DeviceId -> App Command
handleCommand d = return Stay

handleMeasurement :: DeviceId -> Measurement -> App NoContent
handleMeasurement d m@Measurement{..} = return NoContent

handleLogin :: LoginRequest -> App LoginRequest
handleLogin r@LoginRequest{..} = return r
