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
import Data.Maybe                           (catMaybes, fromMaybe)
import Data.Monoid                          ((<>))
import Data.Text                            (Text)
import qualified Data.Text as T
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
import Data.UUID.V4 (nextRandom)
import GHC.Int (Int64)
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus as P
import qualified Prometheus.Metric.GHC as P

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO       as T

import API
import Config
import qualified Database as DB
import Query
import ServerEnv
import Device
import Statistics

startApp :: Config -> IO ()
startApp config = do
  let port = configPort config
      logging = loggingMiddleware config
  newEnv config >>= \case
    Nothing -> pure ()
    Just env -> do
      _ <- P.register P.ghcMetrics
      T.putStrLn $ "Serving on http://" <> configHostname config
                   <> ":" <> (T.pack . show $ port)
      run port . P.prometheus P.def . logging =<< app env

loggingMiddleware :: Config -> Middleware
loggingMiddleware config = case configAccessLogLevel config of
  Disabled    -> id
  Enabled     -> logStdout
  Development -> logStdoutDev

app :: Env -> IO Application
app env = pure $ serveWithContext api (appContext env) (server env)
  where  api = Proxy :: Proxy API

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

server :: Env -> Server API
server env = enter (appToHandler env) api
  where
    api = unprotected :<|> device
    device uuid = command uuid :<|> measurement uuid
    unprotected = login :<|> statistics

    command :: DeviceId -> App CommandAction
    command _ = return $ CommandAction NoChargeBattery NoFillReservoir NoFlushReservoir

    measurement :: DeviceId -> Measurement -> App NoContent
    measurement deviceId m@Measurement{..} = do
      withConnection $ \c -> do
        insertDevice c deviceId
        insertMeasurement c m { measurementDeviceId = deviceId }
      liftIO $ P.incCounter measurementsCounter
      return NoContent

    login :: LoginRequest -> App LoginRequest
    login r@LoginRequest{..} = do
      deviceId <- liftIO nextRandom
      withConnection $ \c -> insertDevice c deviceId
      return $ LoginRequest (UUID.toText deviceId)

statistics :: App Statistics
statistics = Statistics <$>
             oneInt countMeasurementsQuery <*>
             oneInt countDevicesQuery <*>
             (headOr 0 <$> runQueryWithConnection (measurementChargeNow <$> totalsQuery)) <*>
             (headOr 0 <$> runQueryWithConnection (measurementWaterLevel <$> totalsQuery)) <*>
             runQueryWithConnection (deviceAveragesQuery measurementWaterTemp) <*>
             pure 0 -- total water energy, cbb
  where
    oneInt = fmap (longInt . head) . runQueryWithConnection
      where longInt = fromIntegral :: Int64 -> Int
    headOr v = fromMaybe v . headMay

{-# NOINLINE measurementsCounter #-}
measurementsCounter :: P.Metric P.Counter
measurementsCounter = P.unsafeRegisterIO
  $ P.counter
  $ P.Info "app_measurements_total" "The number of measurements submitted to the app."
