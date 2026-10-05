{
  den,
  inputs,
  ...
}:
{
  den.hosts.aarch64-linux.lora-pi = {
    instantiate = den.lib.raspberry-pi-builder;
    users.ivy = {
      # The human behind this account (../../persons/ivy.nix). The link
      # points account -> person, never the reverse; ../../base/person.nix says why.
      person = den.persons.ivy;
    };
  };

  den.aspects.lora-pi = {
    includes = [
      den.aspects.nixos-raspberrypi
    ];
  };

  den.aspects.ivy.provides.lora-pi = {
    includes = [
      den.aspects.fish
      den.aspects.neovim
      den.aspects.gpg
      den.batteries.primary-user
    ];
  };
}
