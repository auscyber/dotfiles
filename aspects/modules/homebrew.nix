{ inputs, lib, ... }:
let
  taps = {
    cask = "homebrew/homebrew-cask";
    core = "homebrew/homebrew-core";
    speedtest = "teamookla/homebrew-speedtest";
    typewhisper = "typewhisper/homebrew-tap";
  };

  tapChanged = lib.mapAttrs (name: value: rec {
    input-name = "homebrew-${name}";
    url = "github:${value}";
    tapName = value;
    input = inputs."${input-name}";

  }) taps;

in

{
  flake-file.inputs = {
    nix-homebrew.url = "github:zhaofengli/nix-homebrew";
  }
  // (lib.mapAttrs' (name: value: {
    name = value.input-name;
    value = {
      url = value.url;
      flake = false;
    };
  }) tapChanged);

  flake.modules.darwin.homebrew = { config, inputs, ... }: {
    imports = [ inputs.nix-homebrew.darwinModule ];

    nix-homebrew = {
      enable = true;
      enableRosetta = true;
      user = config.system.primaryUser;
      taps = lib.mapAttrs' (
        name: value: {
          name = value.tapName;
          value = value.input;
        }
      );
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
    };
  };

}
