{ ... }:
let
  linuxHome = username: {
    homeManager =
      { pkgs, ... }:
      {
        home = {
          inherit username;
          homeDirectory = "/home/${username}";
        };
        auscybernix.standalone.enable = true;
      };
  };
in
{
  # Standalone home configurations for machines that are NOT managed by NixOS
  # or nix-darwin in this flake (e.g. foreign distros, remote VMs, shared
  # machines).  These correspond to the den.homes.* entries in flake/den.nix.
  # Per-host path imports are inlined here because each home is unique to a
  # single (user, host) combination with no shared host aspect.

  den.aspects."auscyber@arch" = (linuxHome "auscyber") // {
    homeManager =
      { pkgs, ... }:
      {
        imports = [ (../../homes/x86_64-linux + "/auscyber@arch") ];
        home = {
          username = "auscyber";
          homeDirectory = "/home/auscyber";
        };
        auscybernix.standalone.enable = true;
      };
  };

  den.aspects."auscyber@laptop" = {
    homeManager =
      { pkgs, ... }:
      {
        imports = [ (../../homes/x86_64-linux + "/auscyber@laptop") ];
        home = {
          username = "auscyber";
          homeDirectory = "/home/auscyber";
        };
        auscybernix.standalone.enable = true;
      };
  };

  den.aspects."ivy@imflopet" = {
    homeManager =
      { pkgs, ... }:
      {
        imports = [ (../../homes/x86_64-linux + "/ivy@imflopet") ];
        home = {
          username = "ivy";
          homeDirectory = "/home/ivy";
        };
        auscybernix.standalone.enable = true;
      };
  };

  den.aspects."ivy@vmi1472413.contaboserver.net" = {
    homeManager =
      { pkgs, ... }:
      {
        imports = [ (../../homes/x86_64-linux + "/ivy@vmi1472413.contaboserver.net") ];
        home = {
          username = "ivy";
          homeDirectory = "/home/ivy";
        };
        auscybernix.standalone.enable = true;
      };
  };
}
