{-# LANGUAGE OverloadedStrings #-}

module Opaleye.Extra
  ( module Opaleye
  , eqNullable
  , stdDevP
  , extractHour
  ) where

import Data.Text (Text)

import Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.Aggregate as A
import           Opaleye.Internal.Aggregate (Aggregator)
import qualified Opaleye.Column    as C
import Opaleye.Internal.Column (Column(Column))
import qualified Opaleye.PGTypes   as T

-- | Returns false when the second input is nullable
eqNullable :: Column a -> Column (Nullable a) -> Column PGBool
eqNullable a = matchNullable (pgBool False) (a .==)

-- | standard deviation - population
stdDevP :: Aggregator (C.Column T.PGFloat8) (C.Column T.PGFloat8)
stdDevP = A.makeAggr (HPQ.AggrOther "stddev_pop")
-- stdDevP = A.makeAggr HPQ.AggrStdDevP

-- | gets the hour of day for a timestamp
extractHour :: C.Column T.PGTimestamptz -> C.Column T.PGInt4
extractHour = toInt . extract (constant ("hour" :: Text))
  where toInt = unsafeCast "INT" :: C.Column T.PGFloat8 -> C.Column T.PGInt4

-- | EXTRACT(w FROM i)
extract :: Column T.PGText -> Column i -> Column e
extract (Column w) (Column i) = Column (HPQ.FunExpr "extract" [w `from` i])
  where from = HPQ.BinExpr (HPQ.OpOther "from")
