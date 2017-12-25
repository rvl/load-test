{ config, pkgs, ... }:

{
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql96;

    # enable network access
    enableTCPIP = true;
    authentication = pkgs.lib.mkAfter ''
      host  all all 192.168.56.1/24 md5
    '';

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
}
