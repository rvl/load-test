# Load testing experiment

This is a NixOps network to experiment with a fictional IOT scaling
scenario.

There are a number of IOT devices out in the field which regularly
send their measurements to a server and poll for commands.

The question is how will the server perform with a large number of
devices and what will be the performance bottleneck.

## Test harness

The test harness has the following components:

- A model data collection service ([`app/src/API.hs`](./app/src/API.hs)). This is a
  Haskell Servant REST API. It has three endpoints:
  - `GET /command` -- the device asks what it should be doing for the
    next time interval.
  - `POST /measurement` -- the device sends its current state to be logged.
  - `GET /stats` -- an example endpoint which calculates statistics on
    the historical dataset. For example: number of measurements, total
    current battery charge.

- A database - PostgreSQL 9.6 configured with the TimeScaleDB plugin.
  For setup, see [`load-test-app.nix`](./load-test-app.nix)
  For schema see [`app/src/migrations/001-load-test.sql`](./app/src/migrations/001-load-test.sql).

- A monitoring environment -- Prometheus and Grafana. See
  [`monitoring.nix`](./monitoring.nix).

- A load testing tool -- [Locust](https://locust.io). See
  [`load-test-locust.nix`](./load-test-locust.nix).

Prometheus is configured to collect and store the following metrics:

- GHC runtime system metrics, especially garbage collector stats.
- Node metrics that can indicate utilization, saturation, errors of
  various resources.
- REST API server metrics, such as request duration.

Locust provides a very convenient web interface to kick off the test.

The [`scripts/locustfile.py`](./scripts/locustfile.py) creates numerous
clients which login to the model app with a random device ID then
repeatedly poll the `/command` endpoint and push random data to the
`/measurement` endpoint.

## Goal of the test harness

The goal is to make a realistic production-like environment for the
app and then feed it data until it fails. The monitoring tools should
keep enough metrics to hint at where the bottlenecks are. The tools
also have attractive and helpful web interfaces.

Another goal is to help understand performance characteristics of the
application. The CSV export from Prometheus will help with this.

## How to run the test harness

To try the test harness on VirtualBox:

    $ nixops create -d load-test load-test-network.nix load-test-vbox.nix
    $ nixops deploy -d load-test

The various tools can be accessed at the following URLs (IP addresses
might vary slightly)

 * Locust: http://192.168.56.101:8089/
 * Prometheus: http://192.168.56.102:9090
 * Grafana: http://192.168.56.102:3000
 * Model App: http://192.168.56.102:8080/metrics
