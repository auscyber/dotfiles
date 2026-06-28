{ den, inputs, ... }:
{
  flake-file.inputs.nixos-hardware.url = "github:NixOS/nixos-hardware";

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
