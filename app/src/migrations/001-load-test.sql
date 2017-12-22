-- Extend the database with TimescaleDB
CREATE EXTENSION IF NOT EXISTS timescaledb CASCADE;

-- UUID generation functions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE device(
  id UUID PRIMARY KEY NOT NULL DEFAULT uuid_generate_v4()
);

CREATE TABLE measurement(
  device_id UUID NOT NULL REFERENCES device(id),
  time timestamptz NOT NULL,
  charge_now INTEGER NOT NULL,
  charge_full INTEGER NOT NULL,
  charge_rate DOUBLE PRECISION NOT NULL,
  ambient_temp DOUBLE PRECISION NOT NULL,
  water_temp DOUBLE PRECISION NOT NULL,
  water_level INTEGER NOT NULL
);

SELECT create_hypertable('measurement', 'time');

CREATE TABLE command(
  device_id UUID NOT NULL REFERENCES device(id),
  time timestamptz NOT NULL,
  charge BOOLEAN NOT NULL,
  fill BOOLEAN NOT NULL,
  flush DOUBLE PRECISION,
  status BOOLEAN
);
