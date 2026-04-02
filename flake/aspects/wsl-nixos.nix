{ ... }:
{
  den.aspects.wsl-nixos = {
    nixos = { ... }: {
      imports = [ ../../systems/x86_64-linux/wsl-nixos ];
    };
    # No dedicated home configuration for wsl-nixos; auscyber's base
    # homeManager aspect applies via den.aspects.auscyber.homeManager.
  };
}
