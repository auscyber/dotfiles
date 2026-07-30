{
  lib,
  inputs,
  ...
}:
{
  den.aspects.secure-boot = {
    # Declared on the aspect, not the file: the partition generator reads
    # which aspect owns an input, and which platforms pull that aspect in.
    flake-file = _: {
      inputs.lanzaboote = {
        url = "github:nix-community/lanzaboote/v1.1.0";

        # Optional but recommended to limit the size of your system closure.
        inputs.nixpkgs.follows = "nixpkgs";
      };
    };

    nixos = { pkgs, ... }: {
      imports = [ inputs.lanzaboote.nixosModules.lanzaboote ];
      environment.systemPackages = [
        # For debugging and troubleshooting Secure Boot.
        pkgs.sbctl
      ];
      boot.loader.systemd-boot.enable = lib.mkForce false;

      boot.lanzaboote = {
        enable = true;
        pkiBundle = "/var/lib/sbctl";
        autoGenerateKeys.enable = true;
        autoEnrollKeys.enable = true;
      };
    };
  };
}
