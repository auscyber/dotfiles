{ den, ... }:
{
  #  den.hosts.aarch64-linux-rpi.lora-pi = {
  #    users.ivy = { };
  #  };
  #
  #  den.aspects.lora-pi = {
  #    includes = [
  #      den.aspects.nixos-raspberrypi
  #    ];
  #  };
  #
  #  den.aspects.ivy.provides.lora-pi = {
  #    includes = [
  #      den.aspects.fish
  #      den.aspects.neovim
  #      den.aspects.gpg
  #      den.batteries.primary-user
  #    ];
  #
  #    homeManager.home.stateVersion = "25.05";
  #  };
}
