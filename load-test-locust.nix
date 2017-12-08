{ config, pkgs, ... }:

let
  locustFile = ./scripts/locustfile.py;
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
      ${pkgs.python3Packages.locustio}/bin/locust -f ${locustFile} --host="http://app:8080" --port=${toString locustPort}
    '';

    serviceConfig = {
      LimitNOFILE = 40000;
    };
  };

}
