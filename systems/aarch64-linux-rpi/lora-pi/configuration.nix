{ config, pkgs, ... }:
{
  users.users.ivy = {
    initialPassword = "abc1234";
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILeCdR16VYTNmoEekYk/b1sskC+trPx9tpOBJoKML17H"
    ];
    shell = pkgs.fish;
  };
  services.accounts-daemon.enable = true;
  networking.hostName = "lora-pi";
  services.openssh.enable = true;
  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };
  networking.useNetworkd = true;

  security.polkit.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILeCdR16VYTNmoEekYk/b1sskC+trPx9tpOBJoKML17H"
  ];
  users.users.root.initialHashedPassword = "";
  nix.settings.trusted-users = [ "ivy" ];
  time.timeZone = "Australia/Melbourne";
  programs.fish.enable = true;
  environment.systemPackages = with pkgs; [
    vim
    git
    tree
    htop
    curl
    wget
  ];

  hardware.raspberry-pi.config = {
    all = {
      options = {
        enable_uart = {
          enable = true;
          value = true;
        };
      };
    };
  };
  system.stateVersion = "25.05";
}
