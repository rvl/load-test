-- Extend the database with TimescaleDB
CREATE EXTENSION IF NOT EXISTS timescaledb CASCADE;

-- UUID generation functions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE device(
  id UUID PRIMARY KEY NOT NULL DEFAULT uuid_generate_v1()
);

CREATE TABLE measurement(
  device_id UUID NOT NULL REFERENCES device(id),
  time timestamptz NOT NULL,
  charge INTEGER NOT NULL,
  max_charge INTEGER NOT NULL,
  charge_rate DOUBLE PRECISION NOT NULL,
  temp DOUBLE PRECISION NOT NULL
);

SELECT create_hypertable('measurement', 'time');

CREATE TABLE command(
  device_id UUID NOT NULL REFERENCES device(id),
  time timestamptz NOT NULL,
  command TEXT NOT NULL,
  status BOOLEAN
);
