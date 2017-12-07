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
import Data.Text
import Database.PostgreSQL.Simple (Connection, withTransaction, SqlError)
import Opaleye.Extra
import Data.Time.Clock (getCurrentTime, UTCTime)
import Control.Exception (catch)
import Control.Monad (void)

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

insertDevice :: Connection -> DeviceId -> IO ()
insertDevice c d = catch (withTransaction c insert) ignore
  where
    insert = void $ runInsertMany c deviceTable [Device (Just (pgUUID d))]
    ignore :: SqlError -> IO ()
    ignore e = return ()

insertMeasurement :: Connection -> Measurement -> IO ()
insertMeasurement c m = void $
  runInsertMany c measurementTable [pgMeasurement m]

countMeasurementsQuery :: Query (Column PGInt8)
countMeasurementsQuery = countRows (queryTable measurementTable)
