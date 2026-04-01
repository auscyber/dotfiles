{ ... }:
{
  den.aspects.auspc = {
    # NixOS configuration for the auspc host.
    nixos = { ... }: {
      imports = [ ../../systems/x86_64-linux/auspc ];
    };

    # Per-host home-manager additions for auscyber on auspc, delivered via
    # the mutual-provider battery (host.provides.user).
    provides.auscyber = { ... }: {
      homeManager.imports = [ (../../homes/x86_64-linux + "/auscyber@auspc") ];
    };
  };
}
