{
  den,
  inputs,
  ...
}:
{
  ff.nixos-hardware.url = "github:NixOS/nixos-hardware";
  # Without this, nixos-hardware pulls its own full nixpkgs (a separate
  # releases.nixos.org nixexprs.tar.xz) rather than reusing ours.
  ff.nixos-hardware.inputs.nixpkgs.follows = "nixpkgs";

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
    nixos.imports = [
      inputs.nixos-hardware.nixosModules.microsoft-surface-common
      inputs.nixos-hardware.nixosModules.microsoft-surface-laptop-amd
    ];
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
