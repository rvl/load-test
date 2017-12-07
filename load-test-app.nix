{ config, pkgs, ... }:

let
  app = import ./app { inherit pkgs; };
  appPort = 8080;
  prometheusPort = 9090;
  grafanaPort = 3000;

in
{

  imports = [
    ./load-test-common.nix
    ./node-exporter.nix
  ];

  networking.firewall.enable = false;
  networking.firewall.allowedTCPPorts = [
    80 appPort
    grafanaPort
    prometheusPort
    9093  # alertmanager
  ];

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql96;
    enableTCPIP = true;
    authentication = pkgs.lib.mkAfter ''
      host  all all 192.168.56.1/24 md5
    '';

    # enable timescaledb
    extraPlugins = [ pkgs.timescaledb ];
    extraConfig = "shared_preload_libraries = 'timescaledb'";
  };

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

  systemd.services.app-setup = {
    description = "Create database";
    serviceConfig.Type = "oneshot";
    serviceConfig.RemainAfterExit = true;
    wantedBy = [ "multi-user.target" ];
    requires = [ "postgresql.service" ];
    after = [ "postgresql.service" ];
    script = ''
      export PATH=${pkgs.postgresql96}/bin:$PATH
      createuser load_test || true
      createdb -O load_test load_test || true
      psql load_test < ${./app/src/migrations/001-load-test.sql} || true
    '';
  };

  systemd.services.app = {
    description = "The app under test";
    wantedBy = [ "multi-user.target" ];
    requires = [ "postgresql.service" "app-setup.service" ];
    after = [ "postgresql.service" "app-setup.service" ];
    script = ''
      ${app}/bin/load-test-server --hostname 0.0.0.0 --port ${toString appPort} --database postgres:///load_test +RTS -T
    '';
  };
}
