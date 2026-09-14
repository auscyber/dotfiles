{
  den,
  inputs,
  ...
}:
{
  den.hosts.x86_64-linux.wsl-nixos = {
    users.nixos = { };
  };

  den.aspects.wsl-nixos = {
    layers = [ "nixos" ];
    inputs.nixos-wsl.url = "github:nix-community/NixOS-WSL/main";
    # A module FUNCTION, not a bare attrset.
    #
    # Class content written as an attrset is evaluated while den COLLECTS it
    # during fleet resolution, so `inputs.nixos-wsl` was forced by anything that
    # touched `config.flake` -- including `darwinConfigurations.<mac>`, which has
    # no business resolving a nixos-layer input. Found by
    # `checks.layer-isolation`. A function is stored and only applied when this
    # host's module system actually runs.
    nixos = _: { imports = [ inputs.nixos-wsl.nixosModules.default ]; };
  };

  den.aspects.nixos = {
    includes = [
      den.aspects.fish
      den.batteries.primary-user
    ];

    provides.to-users.homeManager.programs.git.extraConfig.core.sshCommand = "ssh.exe";
  };
}
