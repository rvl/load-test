{ config, pkgs, ... }:

let
  app = import ./app { inherit pkgs; };
  appPort = 8080;
in
{

  imports = [
    ./load-test-common.nix
    ./monitoring.nix
  ];

  networking.firewall.enable = false;
  networking.firewall.allowedTCPPorts = [
    appPort
  ];

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql96;

    # enable timescaledb
    extraPlugins = [ pkgs.timescaledb ];
    extraConfig = "shared_preload_libraries = 'timescaledb'";
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
