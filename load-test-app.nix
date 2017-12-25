{ config, pkgs, ... }:

let
  app = import ./app { inherit pkgs; };
  appPort = 8080;
  dbHost = null;
in
{

  imports = [
    ./load-test-common.nix
    ./monitoring.nix
    ./load-test-database.nix
  ];

  networking.firewall.enable = false;
  networking.firewall.allowedTCPPorts = [
    appPort
  ];

  systemd.services.app = {
    description = "The app under test";
    wantedBy = [ "multi-user.target" ];
    requires = if dbHost == null then [ "postgresql.service" "app-setup.service" ] else [];
    after = if dbHost == null then [ "postgresql.service" "app-setup.service" ] else [];
    script = ''
      ${app}/bin/load-test-server --hostname 0.0.0.0 --port ${toString appPort} --database postgres://${toString dbHost}/load_test +RTS -T
    '';

    serviceConfig = {
      # needs to handle a lot of connections
      LimitNOFILE = 40000;
    };
  };
}
