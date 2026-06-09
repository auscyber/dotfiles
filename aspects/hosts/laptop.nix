{
  withSystem,
  inputs,
  self,
  den,
  lib,
  __findFile,
  ...
}:
{

  den.hosts.aarch64-darwin.Ivys-MacBook-Pro = {

    hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICTsjq9lMzer6RPeDfXZ9eI1eiMf8b/fteSOb5XC5rBG";
    roles = [
      "study"
      "gui"
      "dev"
    ];
    users.ivypierlot = {
      hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILeCdR16VYTNmoEekYk/b1sskC+trPx9tpOBJoKML17H";
    };
  };

  den.aspects.Ivys-MacBook-Pro = {
    includes = [
      den.aspects.vpn
      den.aspects.homebrew
    ];

    provides.to-users.homeManager.programs.git.enable = true;
    vpn = { };

  };
  den.aspects.ivypierlot = {
    includes = [
      den.aspects.agenix-rekey
      den.aspects.nixvim
      #      den.aspects.kanata
      den.aspects.fish
      den.batteries.primary-user
      <zen>
    ];
  };
}
