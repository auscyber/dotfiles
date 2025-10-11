{
  config,
  pkgs,
  inputs,
  lib,
  ...
}:
{
  disabledModules = [
    "${inputs.stylix}/modules/gnome/nixos.nix"
    "${inputs.stylix}/modules/qt/nixos.nix"
    "${inputs.stylix}/modules/qt/hm.nix"
  ];
  config = {
    # Use iwd instead of wpa_supplicant. It has a user friendly CLI
    networking.wireless.iwd.enable = lib.mkForce false;
    networking.wireless = {
      enable = lib.mkForce true;
      networks."Ivy's iPhone".psk = "12345678";
      extraConfig = "ctrl_interface=DIR=/var/run/wpa_supplicant GROUP=wheel";
      # output ends up in /run/wpa_supplicant/wpa_supplicant.conf
    };
    stylix = {
      image = ../../../backgrounds/phoebebridgers-2.jpg;
      polarity = "dark";
      enable = true;
      homeManagerIntegration.autoImport = false;
    };
    stylix.targets.qt.platform = { };
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
  };
  options = {
    stylix.targets.qt.platform = lib.mkOption {
      type = lib.types.attrsOf null;
      default = { };
    };
  };
}
