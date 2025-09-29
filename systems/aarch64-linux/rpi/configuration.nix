{ config, pkgs, ... }:
{
  users.users.ivy = {
    initialPassword = "abc1234";
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILeCdR16VYTNmoEekYk/b1sskC+trPx9tpOBJoKML17H"
    ];
  };
  networking.hostname = "ivyrpi5";
  services.openssh.enable = true;

}
