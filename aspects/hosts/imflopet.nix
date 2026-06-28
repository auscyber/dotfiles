{ den, ... }:
{
  den.hosts.x86_64-linux.imflopet = {
    users.ivy = {
      hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOwzECwhRtEus12VIOPw8UrOkBuBwH69VKodEWEuXAsX ivy@imflopet";
    };
  };

  den.aspects.imflopet = { };

  den.aspects.ivy.provides.imflopet = {
    includes = [
      den.aspects.fish
      den.aspects.neovim
      den.aspects.gpg
      den.batteries.primary-user
    ];

    homeManager = {
      programs.git.extraConfig.safe.directory = [
        "/nixos-config"
        "/nixos-config/services/loft"
      ];
      programs.gpg.settings."use-agent" = "";
    };
  };
}
