{ lib, inputs, ... }:
{
  ff.lanzaboote = {
    url = "github:nix-community/lanzaboote/v1.1.0";

    # Optional but recommended to limit the size of your system closure.
    inputs.nixpkgs.follows = "nixpkgs";

  };

  den.aspects.secure-boot = {
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
