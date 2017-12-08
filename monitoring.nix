{ config, pkgs, ... }:

let
  prometheusPort = 9090;
  grafanaPort = 3000;
  appPort = 8080;
in
{

  imports = [
    ./node-exporter.nix
  ];

  networking.firewall.allowedTCPPorts = [
    grafanaPort
    prometheusPort
    9093  # alertmanager
  ];

  # globin's example
  # https://gist.github.com/globin/02496fd10a96a36f092a8e7ea0e6c7dd
  services.prometheus = {
    enable = true;
    listenAddress = "0.0.0.0:${toString prometheusPort}";
    # alertmanager.port = 9093;

    globalConfig = {
      scrape_interval =  "10s";
      evaluation_interval = "10s";
    };
    scrapeConfigs = [
      {
        job_name = "prometheus";
        static_configs = [
          {
            targets = [ "localhost:${toString prometheusPort}" ];
          }
        ];
      }
      {
        job_name = "node";
        static_configs = [
          {
            targets = [ "localhost:9100" ];
          }
        ];
      }
      {
        job_name = "app";
        static_configs = [
          {
            targets = [ "localhost:${toString appPort}" ];
          }
        ];
      }
    ];
  };

  services.grafana = {
    enable = true;
    addr = "0.0.0.0";
    port = grafanaPort;
    domain = "app";
    rootUrl = "http://app:${toString grafanaPort}/";
    auth.anonymous.enable = true;
  };
}
