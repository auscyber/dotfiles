{ den, ... }:
{
  den.hosts.x86_64-linux.auspc = {
    hostPublickey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFYM1mZ3fYfOjhyMhIiKbUOLYTQifG82P2NnGWHyIwHt root@nixos";
    gpu = "nvidia";
    users.auscyber = {
      roles = [
        "gui"
        "gaming"
        "dev"
      ];
      hostPublickey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMOJWDxkmOeVGAq7WcPI+BygJ2zbsn4J0UAq0R6B6ZVx auscyber@auspc";
    };

  };

  den.aspects.auspc = {

    includes = [ den.aspects.vpn ];
  };

  den.aspects.auscyber = {
    includes = [ den.aspects.fish ];
    provides.auspc = {
      includes = [ den.batteries.primary-user ];

    };
  };

}
