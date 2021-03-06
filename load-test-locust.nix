{ config, pkgs, ... }:

let
  scripts = ./scripts;
  locustPort = 8089;

in {
  imports = [  ./load-test-common.nix ];

  environment.systemPackages = with pkgs; [
    python3Packages.locustio
  ];

  networking.firewall.enable = false;
  networking.firewall.allowedTCPPorts = [ locustPort ];

  systemd.services.locust = {
    description = "Load tester";
    wantedBy = [ "multi-user.target" ];
    script = ''
      ${pkgs.python3Packages.locustio}/bin/locust -f ${scripts}/locustfile.py --host="http://app:8080" --port=${toString locustPort}
    '';

    serviceConfig = {
      # needs to make a lot of connections
      LimitNOFILE = 40000;
    };
  };

}
