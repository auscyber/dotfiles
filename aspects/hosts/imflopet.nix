{ den, ... }: {
  den.homes.x86_64-linux."ivy@imflopet" = {
    # The human behind this account (../persons/ivy.nix). The link points
    # account -> person, never the reverse; ../base/person.nix says why.
    person = den.persons.ivy;
    hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOwzECwhRtEus12VIOPw8UrOkBuBwH69VKodEWEuXAsX ivy@imflopet";
  };

  den.aspects.imflopet = { };

  den.aspects.ivy.provides.imflopet = {
    includes = [
      den.aspects.fish
      den.aspects.neovim
      den.aspects.dev
      den.aspects.gpg
      den.batteries.primary-user
    ];

    provides.to-users.homeManager = {
      programs.git.extraConfig.safe.directory = [
        "/nixos-config"
        "/nixos-config/services/loft"
      ];
      programs.gpg.settings."use-agent" = "";
    };
  };
}
