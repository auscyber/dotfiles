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
  ff = {
    nix-homebrew.url = "github:zhaofengli/nix-homebrew";
  };
  den.policies.route-casks =
    { ... }:
    let
      inherit (den.lib.policy) pipe;
    in
    [
      (pipe.from "brew" [
        pipe.collect
        ({ host, ... }: true)
        pipe.expose
      ])
    ];

  den.default.includes = [ den.policies.route-casks ];

  den.quirks.brew.description = "Brew packages, casks and taps";
  den.schema.flake.include = [ den.policies.route-taps ];

  den.aspects.homebrew = {
    includes = [ den.policies.route-casks ];
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
      let
        casks = builtins.concatMap (brew: brew.casks or [ ]) brew;
        brews = builtins.concatMap (brew: brew.brews or [ ]) brew;

        tapNames = builtins.attrNames config.nix-homebrew.taps;

        # Homebrew implicitly trusts `homebrew/*` and `brew trust --tap` refuses
        # to record an official tap ("Official tap ... is always trusted"), so
        # only the third-party ones are worth an entry.
        officialTap = name: builtins.head (lib.splitString "/" name) == "homebrew";
        thirdPartyTaps = builtins.filter (name: !officialTap name) tapNames;

        # `brew trust --formula/--cask` only accepts fully-qualified
        # `user/repo/name`; a bare `speedtest` is resolved through whatever taps
        # happen to be installed and cannot be trusted on its own. Same rule
        # nix-darwin applies when it decides whether a Brewfile `trusted: true`
        # does anything.
        qualified = builtins.filter (
          name: builtins.isString name && builtins.length (lib.splitString "/" name) == 3
        );
      in
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
          # nix-homebrew runs `brew trust` during its own setup step, which is
          # *before* nix-darwin runs `brew bundle`. Necessary but not sufficient
          # -- see the `homebrew.taps` note below for why the Brewfile has to
          # carry the same trust.
          trust = {
            taps = thirdPartyTaps;
            formulae = qualified brews;
            casks = qualified casks;
          };
          mutableTaps = false;
          autoMigrate = true;
        };
        homebrew = {
          # Trust has to be declared *here*, not just in `nix-homebrew.trust`
          # above. `onActivation.cleanup != "none"` makes nix-darwin invoke
          # `brew bundle --force-cleanup`, and brew's cleanup does
          # `Homebrew::Trust.replace!(Homebrew::Bundle::Trust.entries(...))` --
          # it *replaces* ~/.homebrew/trust.json with exactly what the Brewfile
          # declares, discarding everything nix-homebrew trusted minutes
          # earlier. So whatever is not `trusted: true` in the Brewfile is not
          # trusted at all, and since Homebrew 6.0.0 defaults
          # HOMEBREW_REQUIRE_TAP_TRUST=1 that means its formulae are refused and
          # its casks silently skipped on the next activation.
          #
          # nix-darwin already defaults `trusted = true` on brews/casks, but
          # that only bites for fully-qualified names -- a bare `speedtest` or
          # `typewhisper` produces no trust entry, so the trust must come from
          # the containing tap instead. Hence: mark every third-party tap we
          # declare as trusted, which is the same blanket trust
          # `nix-homebrew.trust.taps` was already asking for.
          taps = map (name: {
            inherit name;
            trusted = !officialTap name;
          }) tapNames;
          enable = true;
          inherit casks brews;
          onActivation = {
            autoUpdate = true; # Fetch the newest stable branch of Homebrew's git repo
            upgrade = true; # Upgrade outdated casks, formulae, and App Store apps
            # 'zap': uninstalls all formulae(and related files) not listed in the generated Brewfile
            cleanup = "uninstall";
          };
        };
      };
  };
}
