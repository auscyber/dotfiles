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
{
  imports = [
    (lib.inputMeta {
      addRegistry = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Add this input to the registry";
      };
    })
  ];
  ff = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    # all your other inputs
  };

  # A convenience `nix` class, forwarded into the host's `nix.*` on both NixOS
  # and nix-Darwin. Modeled exactly on the built-in `os` class (`os-to-host`)
  # and `user` class (`user-to-host`): the routing is a *policy*, not a
  # forward-aspect.
  #
  # Why a policy and not `den.{provides,batteries}.forward`: a forward returns an
  # aspect that only applies to the scope tree it's included in. Even placed in
  # `den.default.includes` it reaches only the *host* default aspect tree -- so
  # `nix.*` written by host aspects (this file's `nix`/`extra-registry`) is
  # forwarded, but `nix.*` written by *user*-included aspects (e.g. `idris`,
  # pulled in via the `ivypierlot` user) lives in the separate user scope tree
  # and is silently dropped. That was the bug: `idris`'s `trusted-substituters`
  # (the `gh-nix-idris2-packages` cache) never reached the host's `nix.settings`.
  #
  # A policy is the only construct the policy machinery replicates into *every*
  # scope where a host is bound -- including user scopes -- so it captures
  # `nix.*` from host aspects and user aspects alike and routes it to the host's
  # actual class. `path = [ "nix" ]` re-nests the class content (e.g.
  # `nix.settings.foo`) under the target's `nix` option (`darwin.nix.settings.foo`).
  den.classes.nix.description = "Convenience class forwarding to the host's nix.* (nixos/darwin)";

  den.policies.nix-to-host =
    { host, ... }:
    # A synthetic `user@host` home with no declared host has no OS class to route
    # into; `host ? class` gates the route to real hosts (mirrors os-to-host).
    lib.optional
      (
        host ? class
        && builtins.elem host.class [
          "nixos"
          "darwin"
        ]
      )
      (den.lib.policy.route {
        fromClass = "nix";
        intoClass = host.class;
        path = [ "nix" ];
      });

  den.aspects.extra-registry = {
    # `nix.registry` here is picked up by the `nix-to-host` policy above (no
    # local `include` needed -- the policy fires in every host-bound scope).
    nix.registry =
      config.flake-file.inputsWithMeta
      |> lib.filterAttrs (_: v: v.meta.addRegistry)
      |> lib.mapAttrs (k: v: { flake = inputs.${k}; });
  };

  den.default.includes = [
    den.policies.nix-to-host
    den.aspects.nix
    den.aspects.extra-registry
  ];

  den.aspects.nix = {
    includes = [
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
      #      settings.experimental-features = [
      #        "nix-command"
      #        "flakes"
      #      ];
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
