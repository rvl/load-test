{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Database where

import qualified Data.Aeson                 as JSON
import           Data.Profunctor
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Scientific
import           Data.Text
import           Data.Time
import           Data.UUID
import           GHC.Int
import           Opaleye hiding (fromNullable)

-- | A newtype around @a -> Maybe b@ to facilitate conversions from the
-- Nullable types.
newtype ToMaybe a b = ToMaybe { unToMaybe :: a -> Maybe b }

instance Profunctor ToMaybe where
  dimap f g (ToMaybe h) = ToMaybe (fmap g . h . f)

instance ProductProfunctor ToMaybe where
  empty = ToMaybe pure
  (ToMaybe f) ***! (ToMaybe g) = ToMaybe (\(x, y) -> (,) <$> f x <*> g y)

-- | This instance makes sure that values which are required in the output are
-- required in the input.
instance Default ToMaybe (Maybe a) a where
  def = ToMaybe id

-- | This instance allows values which are optional in the output to be
-- optional in the input.
instance Default ToMaybe (Maybe a) (Maybe a) where
  def = ToMaybe pure

-- | Convert from any Nullable type by "sequencing" over all the fields.
fromNullable :: Default ToMaybe a b => a -> Maybe b
fromNullable = unToMaybe def

---- Types for table: command ----

data Command' c1 c2 c3 c4 c5 c6 =
  Command
    { commandDeviceId :: c1
    , commandTime :: c2
    , commandCharge :: c3
    , commandFill :: c4
    , commandFlush :: c5
    , commandStatus :: c6
    }

type Command = Command' UUID UTCTime Bool Bool (Maybe Double) (Maybe Bool)

type CommandReadColumns = Command' (Column PGUuid) (Column PGTimestamptz) (Column PGBool) (Column PGBool) (Column (Nullable PGFloat8)) (Column (Nullable PGBool))

type CommandWriteColumns = Command' (Column PGUuid) (Column PGTimestamptz) (Column PGBool) (Column PGBool) (Maybe (Column (Nullable PGFloat8))) (Maybe (Column (Nullable PGBool)))

type CommandNullableColumns = Command' (Column (Nullable PGUuid))(Column (Nullable PGTimestamptz)) (Column (Nullable PGBool)) (Column (Nullable PGBool)) (Column (Nullable PGFloat8)) (Column (Nullable PGBool))

type CommandNullable = Command' (Maybe UUID) (Maybe UTCTime) (Maybe Bool) (Maybe Bool) (Maybe Double) (Maybe Bool)

fromNullableCommand :: CommandNullable -> Maybe Command
fromNullableCommand = fromNullable

$(makeAdaptorAndInstance "pCommand" ''Command')

commandTable :: Table CommandWriteColumns CommandReadColumns
commandTable = Table "command" (pCommand
  Command
    { commandDeviceId = required "device_id"
    , commandTime = required "time"
    , commandCharge = required "charge"
    , commandFill = required "fill"
    , commandFlush = optional "flush"
    , commandStatus = optional "status"
    }
  )

---- Types for table: device ----

data Device' c1 =
  Device
    { deviceId :: c1
    }

type Device = Device' UUID

type DeviceReadColumns = Device' (Column PGUuid)

type DeviceWriteColumns = Device' (Maybe (Column PGUuid))

type DeviceNullableColumns = Device' (Column (Nullable PGUuid))

type DeviceNullable = Device' (Maybe UUID)

fromNullableDevice :: DeviceNullable -> Maybe Device
fromNullableDevice = fromNullable

$(makeAdaptorAndInstance "pDevice" ''Device')

deviceTable :: Table DeviceWriteColumns DeviceReadColumns
deviceTable = Table "device" (pDevice
  Device
    { deviceId = optional "id"
    }
  )

---- Types for table: measurement ----

data Measurement' c1 c2 c3 c4 c5 c6 c7 c8 =
  Measurement
    { measurementDeviceId :: c1
    , measurementTime :: c2
    , measurementChargeNow :: c3
    , measurementChargeFull :: c4
    , measurementChargeRate :: c5
    , measurementAmbientTemp :: c6
    , measurementWaterTemp :: c7
    , measurementWaterLevel :: c8
    }

type Measurement = Measurement' UUID UTCTime Int Int Double Double Double Int

type MeasurementReadColumns = Measurement' (Column PGUuid) (Column PGTimestamptz) (Column PGInt4) (Column PGInt4) (Column PGFloat8) (Column PGFloat8) (Column PGFloat8) (Column PGInt4)

type MeasurementWriteColumns = Measurement' (Column PGUuid) (Column PGTimestamptz) (Column PGInt4) (Column PGInt4) (Column PGFloat8) (Column PGFloat8) (Column PGFloat8) (Column PGInt4)

type MeasurementNullableColumns = Measurement' (Column (Nullable PGUuid)) (Column (Nullable PGTimestamptz)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGFloat8)) (Column (Nullable PGFloat8)) (Column (Nullable PGFloat8)) (Column (Nullable PGInt4))

type MeasurementNullable = Measurement' (Maybe UUID) (Maybe UTCTime) (Maybe Int) (Maybe Int) (Maybe Double) (Maybe Double) (Maybe Double) (Maybe Int)

fromNullableMeasurement :: MeasurementNullable -> Maybe Measurement
fromNullableMeasurement = fromNullable

$(makeAdaptorAndInstance "pMeasurement" ''Measurement')

measurementTable :: Table MeasurementWriteColumns MeasurementReadColumns
measurementTable = Table "measurement" (pMeasurement
  Measurement
    { measurementDeviceId = required "device_id"
    , measurementTime = required "time"
    , measurementChargeNow = required "charge_now"
    , measurementChargeFull = required "charge_full"
    , measurementChargeRate = required "charge_rate"
    , measurementAmbientTemp = required "ambient_temp"
    , measurementWaterTemp = required "water_temp"
    , measurementWaterLevel = required "water_level"
    }
  )

----------------------------------------------------------------------------
-- not autogenerated

pgMeasurement :: Measurement -> MeasurementWriteColumns
pgMeasurement = pMeasurement Measurement
    { measurementDeviceId     = pgUUID
    , measurementTime         = pgUTCTime
    , measurementChargeNow    = pgInt4
    , measurementChargeFull   = pgInt4
    , measurementChargeRate   = pgDouble
    , measurementAmbientTemp  = pgDouble
    , measurementWaterTemp    = pgDouble
    , measurementWaterLevel   = pgInt4
    }

instance Default Constant Measurement MeasurementWriteColumns where
   def = Constant pgMeasurement


pgNullableText :: Maybe Text -> Maybe (Column (Nullable PGText))
pgNullableText = fmap (toNullable . pgStrictText)
