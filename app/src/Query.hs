{-# LANGUAGE Arrows          #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Query
  ( insertMeasurement
  , insertDevice
  , countMeasurementsQuery
  , commandForDeviceQuery
  ) where

import Control.Arrow              (returnA)
import Data.ByteString
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, withTransaction, SqlError)
import Opaleye.Extra
import Data.Time.Clock (getCurrentTime, UTCTime)
import Control.Exception (catch)
import Control.Monad (void, when)

import Database
import Device

-- | A query to get a user from their id
commandForDeviceQuery :: DeviceId -> UTCTime -> Query CommandReadColumns
commandForDeviceQuery uuid now = proc () -> do
  cmd@Command{..} <- queryTable commandTable -< ()
  restrict -< pgUUID uuid .== commandDeviceId
  restrict -< pgUTCTime now .>= commandTime
  restrict -< isNull commandStatus
  returnA -< cmd

deviceByIdQuery :: DeviceId -> Query DeviceReadColumns
deviceByIdQuery uuid = proc () -> do
  d <- queryTable deviceTable -< ()
  restrict -< pgUUID uuid .== deviceId d
  returnA -< d

insertDevice :: Connection -> DeviceId -> IO ()
insertDevice c d = withTransaction c $ do
  ds <- runQuery c (deviceByIdQuery d) :: IO [Device]
  when (Prelude.null ds) $ void $
    runInsertMany c deviceTable [Device (Just (pgUUID d))]

insertMeasurement :: Connection -> Measurement -> IO ()
insertMeasurement c m = void $
  runInsertMany c measurementTable [pgMeasurement m]

countMeasurementsQuery :: Query (Column PGInt8)
countMeasurementsQuery = countRows (queryTable measurementTable)
