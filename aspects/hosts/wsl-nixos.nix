{ den, inputs, ... }:
{
  flake-file.inputs.nixos-wsl.url = "github:nix-community/NixOS-WSL/main";

  den.hosts.x86_64-linux.wsl-nixos = {
    users.nixos = { };
  };

  den.aspects.wsl-nixos = {
    nixos.imports = [ inputs.nixos-wsl.nixosModules.default ];
  };

  den.aspects.nixos = {
    includes = [
      den.aspects.fish
      den.batteries.primary-user
    ];

    provides.to-users.homeManager.programs.git.extraConfig.core.sshCommand = "ssh.exe";
  };
}
