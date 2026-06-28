{
  inputs,
  lib,
  den,
  ...
}:
let

  tapChanged = lib.mapAttrs (
    name: value: rec {
      input-name = "homebrew-${name}";
      url = "github:${value}";
      tapName = value;
      input = inputs."${input-name}";

    }
  );

in

{
  flake-file.inputs = {
    nix-homebrew.url = "github:zhaofengli/nix-homebrew";
  };

  den.quirks.brew.description = "Brew packages, casks and taps";
  den.schema.flake.include = [ den.policies.route-taps ];

  den.aspects.homebrew = {
    brew.taps = {
      cask = "homebrew/homebrew-cask";
      core = "homebrew/homebrew-core";
      speedtest = "teamookla/homebrew-speedtest";
      typewhisper = "typewhisper/homebrew-tap";
    };
    flake-file = { brew, ... }: {
      inputs = lib.listToAttrs (
            lib.concatMap (
              x:
              if x ? taps then
                lib.mapAttrsToList (_: value: {
                  name = value.input-name;
                  value = {
                    url = value.url;
                    flake = false;
                  };
                }) (tapChanged x.taps)
              else
                [ ]
            ) brew
          );
    };

    darwin =
      {
        brew,
        config,
        inputs',
        ...
      }:
      {
        imports = [ inputs.nix-homebrew.darwinModules.default ];

        nix-homebrew = {
          enable = true;
          enableRosetta = true;
          user = config.system.primaryUser;
          taps = lib.listToAttrs (
            lib.concatMap (
              x:
              if x ? taps then
                lib.mapAttrsToList (_: value: {
                  name = value.tapName;
                  value = inputs."${value.input-name}";
                }) (tapChanged x.taps)
              else
                [ ]
            ) brew
          );
          mutableTaps = false;
          autoMigrate = true;

        };
        homebrew = {
          taps = builtins.attrNames config.nix-homebrew.taps;
          enable = true;
          casks = builtins.concatMap (brew: brew.casks or [ ]) brew;
          brews = builtins.concatMap (brew: brew.brews or [ ]) brew;
          onActivation = {
            autoUpdate = true; # Fetch the newest stable branch of Homebrew's git repo
            upgrade = true; # Upgrade outdated casks, formulae, and App Store apps
            # 'zap': uninstalls all formulae(and related files) not listed in the generated Brewfile
#            cleanup = "uninstall";
          };
        };
      };
  };

}
