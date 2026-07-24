{
  lib,
  den,
  __find_file,
  rootPath,
  config,
  inputs,
  self,
  ...
}:
let
  nixClass =
    {
      class,
      aspect-chain,
      ...
    }:
    den.provides.forward {
      each = [
        "nixos"
        "darwin"
      ];
      fromClass = _: "nix";
      intoClass = lib.id;
      intoPath = _: [
        "nix"
      ];
      #      fromAspect = _: lib.head aspect-chain;
      adaptArgs = lib.id;
    };
in
{
  imports = [
    (lib.inputMeta {
      addRegistry = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Add this input to the registry";
      };

    })

  ];
  flake-file.inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    # all your other inputs
  };

  den.aspects.extra-registry = {

    includes = [ nixClass ];
    nix.registry =
      config.flake-file.inputsWithMeta
      |> lib.filterAttrs (_: v: v.meta.addRegistry)
      |> lib.mapAttrs (k: v: { flake = inputs.${k}; });

  };

  den.default.includes = [
    den.aspects.nix
    den.aspects.extra-registry
  ];

  den.aspects.nix = {
    includes = [
      nixClass
      # The pipe-operator experimental feature is named differently by each nix
      # implementation -- Lix calls it `pipe-operator` (singular), CppNix
      # `pipe-operators` (plural). The wrong name fails nix.conf validation
      # (`nix.conf.drv`: "unknown experimental feature ..."), so pick the name by
      # whether the `lix` aspect is present on the host. Injected into the `os`
      # class so it reaches both nixos and darwin `nix.settings`; list-valued
      # options merge by concatenation, so this appends to the base list below.
      (den.lib.whenAspect den.aspects.lix {
        os.nix.settings.experimental-features = [
          "pipe-operator"
          "flake-self-attrs"
        ];
      })
      (den.lib.unlessAspect den.aspects.lix {
        os.nix.settings.experimental-features = [ "pipe-operators" ];
      })
    ];
    nixos.nix.settings.trusted-users = [ "@wheel" ];
    os.nix = {
      gc.automatic = true;
      channel.enable = false;
      settings.experimental-features = [
        "nix-command"
        "flakes"
      ];
    };

    nix.registry.dotfiles.flake = inputs.self;

    nix.settings = {
      extra-experimental-features = [
        "nix-command"
        "flakes"
      ];
    };
    darwin = {
      nix.checkConfig = false;
      nix.enable = true;
      nix.settings.trusted-users = [ "@admin" ];
      nix.settings.auto-optimise-store = true;
      nix.optimise = {
        automatic = true;
        interval = [
          {
            Hour = 4;
            Minute = 15;
            Weekday = 7;
          }
        ];
      };
    };
  };
}
