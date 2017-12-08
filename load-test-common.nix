{
  i18n.defaultLocale = "en_US.UTF-8";

  services.nixosManual.showManual = false;
  services.ntp.enable = false;
  services.openssh.allowSFTP = false;
  services.openssh.passwordAuthentication = false;

  users = {
    mutableUsers = false;
    users.root.openssh.authorizedKeys.keyFiles = [ ~/.ssh/id_rsa.pub ];
  };

  # allows locust to spawn lots of connections
  boot.kernel.sysctl = {
    "fs.file-max" = "100000";
  };
}
