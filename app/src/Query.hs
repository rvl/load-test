{-# LANGUAGE Arrows          #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Query
  ( insertMeasurement
  , insertDevice
  , countMeasurementsQuery
  , countDevicesQuery
  , commandForDeviceQuery
  , latestMeasurementsQuery
  , deviceAveragesQuery
  , totalsQuery
  ) where

import Prelude hiding (max, sum)
import Control.Arrow              (returnA)
import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, withTransaction, SqlError)
import Opaleye.Extra
import Data.Time.Clock (getCurrentTime, UTCTime)
import Control.Exception (catch)
import Control.Monad (void, when)
import Data.Profunctor.Product (p2)

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

countDevicesQuery :: Query (Column PGInt8)
countDevicesQuery = countRows (queryTable deviceTable)

totalsQuery :: Query (Measurement' (Column PGInt8) (Column PGTimestamptz) (Column PGInt4) (Column PGInt4) (Column PGFloat8) (Column PGFloat8) (Column PGFloat8) (Column PGInt4))
totalsQuery = aggregate (pMeasurement Measurement
                          { measurementDeviceId    = count
                          , measurementTime        = max
                          , measurementChargeNow   = sum
                          , measurementChargeFull  = sum
                          , measurementChargeRate  = avg
                          , measurementAmbientTemp = avg
                          , measurementWaterTemp   = avg
                          , measurementWaterLevel  = sum
                          }) latestMeasurementsQuery

groupedMeasurements :: Query (Measurement' (Column PGUuid) (Column PGTimestamptz) (Column PGInt8) (Column PGInt8) (Column PGInt8) (Column PGInt8) (Column PGInt8) (Column PGInt8))
groupedMeasurements = aggregate (pMeasurement Measurement
                                 { measurementDeviceId    = groupBy
                                 , measurementTime        = max
                                 , measurementChargeNow   = count
                                 , measurementChargeFull  = count
                                 , measurementChargeRate  = count
                                 , measurementAmbientTemp = count
                                 , measurementWaterTemp   = count
                                 , measurementWaterLevel  = count
                                 }) (queryTable measurementTable)

latestMeasurementsQuery :: Query MeasurementReadColumns
latestMeasurementsQuery = proc () -> do
  a <- queryTable measurementTable -< ()
  b <- groupedMeasurements -< ()
  restrict -< measurementTime a .== measurementTime b
  restrict -< measurementDeviceId a .== measurementDeviceId b
  returnA -< a

measurementHours :: (MeasurementReadColumns -> Column PGFloat8) -> Query (Column PGInt4, Column PGFloat8)
measurementHours f = proc () -> do
  m <- queryTable measurementTable -< ()
  returnA -< (extractHour (measurementTime m), f m)

deviceAveragesQuery :: (MeasurementReadColumns -> Column PGFloat8) -> Query (Column PGInt4, (Column PGFloat8, Column PGFloat8))
deviceAveragesQuery f = aggregate (p2 (groupBy, (,) <$> avg <*> stdDevP)) (measurementHours f)
