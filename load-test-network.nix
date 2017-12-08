{
  network.description = "Load test test";

  app = import ./load-test-app.nix;
  locust = import ./load-test-locust.nix;
}
