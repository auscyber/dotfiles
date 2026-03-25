{
  config,
  inputs,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.homebrew;
in
{
  options.auscybernix.homebrew = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to enable Homebrew on macOS.";
    };
    casks = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];

      description = "List of Homebrew casks to install.";
    };
    brews = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];

      description = "List of Homebrew formulae to install.";
    };
    masApps = lib.mkOption {
      type = lib.types.attrsOf lib.types.int;
      default = { };

      description = "List of Mac App Store apps to install (requires mas-cli).";
    };
  };
  config = lib.mkIf cfg.enable {
    nix-homebrew = {
      enable = true;
      enableRosetta = true;
      user = config.system.primaryUser;
      taps = {
        "homebrew/homebrew-cask" = inputs.homebrew-cask;
        "homebrew/homebrew-core" = inputs.homebrew-core;
        "teamookla/homebrew-speedtest" = inputs.homebrew-speedtest;
      };

      mutableTaps = false;
      autoMigrate = true;

    };
    homebrew = {
      taps = builtins.attrNames config.nix-homebrew.taps;
      enable = true;
      onActivation = {
        autoUpdate = true; # Fetch the newest stable branch of Homebrew's git repo
        upgrade = true; # Upgrade outdated casks, formulae, and App Store apps
        # 'zap': uninstalls all formulae(and related files) not listed in the generated Brewfile
        cleanup = "uninstall";
      };
      casks = cfg.casks;
      brews = cfg.brews;
      masApps = cfg.masApps;
    };
  };
}
