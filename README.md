# Load testing experiment

This is a NixOps network and model application to experiment with a
hypothetical IOT scaling scenario:

> There are a number of devices out in the field which regularly send
> their measurements to a server and poll for commands.

The question is how will the server perform with a large number of
devices and what will be the performance bottleneck.

More discussion in [my blog post](https://rodney.id.au/posts/2017-12-23-nixos-load-tests/).

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

## Setting up Grafana

Grafana doesn't have a simple JSON export/import for data sources, so
there is some manual setup to do.

1. Sign in to Grafana with username `admin`, password `admin`.
2. Add a data source
    * Name: prometheus
    * Default: Yes
    * Type: Prometheus
    * URL: http://localhost:9090
    * Access: Proxy
    * Scrape interval: 10s
3. Import a Dashboard
    * Upload the file [`grafana/dashboard.json`](./grafana/dashboard.json).
4. View the dashboard, etc. Upload the dashboard to
   [raintank.io](https://raintank.io) by using the share button at the
   top of the page and selecting *Snapshot*.
