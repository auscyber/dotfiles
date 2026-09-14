{
  den,
  inputs,
  ...
}:
{
  den.hosts.x86_64-linux.surfacelaptop = {
    roles = [
      "gui"
      "dev"
    ];
    users.auscyber = {
      hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEGAd35TCPkGwoAExQbajnzKC9eMf52ZYqc0kYEF7i5G auscyber@ivyslaptop";
      roles = [
        "gui"
        "dev"
      ];
    };
  };

  den.aspects.surfacelaptop = {
    layers = [ "nixos" ];

    # Declared once, in ../nixos/nvidia.nix -- den rejects the same input being
    # declared by two aspects, identical specs included.
    includes = [ den.aspects.nixos-hardware ];

    # A module FUNCTION: a bare attrset here is evaluated while den COLLECTS
    # class content during fleet resolution, so `inputs.nixos-hardware` -- a
    # nixos-layer input -- was forced by darwin evaluations too. Same fix as
    # ./wsl-nixos.nix; found by `checks.layer-isolation`.
    nixos = _: {
      imports = [
        inputs.nixos-hardware.nixosModules.microsoft-surface-common
        inputs.nixos-hardware.nixosModules.microsoft-surface-laptop-amd
      ];
    };
  };

  den.aspects.auscyber.provides.surfacelaptop = {
    includes = [
      den.aspects.fish
      den.aspects.neovim
      den.aspects.gpg
      den.batteries.primary-user
    ];
  };
}
