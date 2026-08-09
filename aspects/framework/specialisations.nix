# Specialisations declared as compositions of aspects.
#
# Any aspect can contribute to a named specialisation of whatever host (or
# standalone home) it lands on, with the same vocabulary the entity itself is
# written in:
#
#   den.aspects.Ivys-MacBook-Pro.specialisations.assessment = {
#     excludes = [ den.aspects.paneru den.aspects.kanata den.aspects.vpn ];
#     includes = [ den.aspects.exam-tools ];   # additions are ordinary aspects
#     darwin = { ... };                        # ...as is inline class content
#   };
#
# The specialised configuration is then the entity's aspect tree PLUS every
# contribution MINUS every excluded aspect, resolved from scratch
# (../../lib/specialisations.nix) -- not the parent config with `mkForce false`
# sprinkled over the bits that were meant to go away. Add a new bar/daemon aspect
# and a lockdown profile that excludes its provider keeps locking it down; an
# override list would not.
#
# Contributions to the same name from different aspects are unioned, so a
# specialisation is itself a composition rather than something one file owns.
#
# Scope note: `specialisations` written by a *user*-scoped aspect (anything under
# `den.aspects.<user>`) contributes to that user's HOST profile -- the host's
# re-resolve walks its user scopes, so a lockdown profile that excludes a
# user-level aspect drops it from `home-manager.users.<u>` too. Standalone homes
# (`den.homes`) get their own home-manager specialisation.
{
  lib,
  den,
  inputs,
  ...
}:
let
  specialisations = import ../../lib/specialisations.nix { inherit lib den inputs; };

  # Where the routed class content lands on the entity's own module system. Every
  # contribution to a name is kept (list), rather than the last definition
  # winning -- that is what makes several aspects able to shape one profile.
  contributionsOption = lib.mkOption {
    internal = true;
    visible = false;
    default = { };
    type = lib.types.attrsOf (
      lib.types.coercedTo lib.types.raw (v: [ v ]) (lib.types.listOf lib.types.raw)
    );
    description = "Aspect-shaped specialisation contributions collected from the entity's aspect tree.";
  };

  # A specialisation's eval re-collects this same class, so the delivery has to
  # go inert inside one (see lib/specialisations.nix `__isSpecialisation`).
  active = entity: config: !(entity.__isSpecialisation or false) && config.denSpecialisations != { };

  # name -> { configuration; package; }, for every declared specialisation.
  builtByName =
    kind: entity: config:
    lib.mapAttrs (
      name: contributions:
      specialisations.buildFor {
        inherit
          kind
          entity
          name
          contributions
          ;
      }
    ) config.denSpecialisations;

  # `mkdir -p`, not `mkdir`: harmless if the directory is already there.
  linkCommands =
    built:
    ''
      mkdir -p $out/specialisation
    ''
    + lib.concatStringsSep "\n" (
      lib.mapAttrsToList (
        name: spec: "ln -sfn ${spec.package} $out/specialisation/${lib.escapeShellArg name}"
      ) built
    );

  # The hand-written `specialisation.<name>.configuration` surface (the darwin
  # patch in patches/darwin/specialisation.patch, home-manager's
  # modules/misc/specialisation.nix) writes its own builder commands with a plain
  # `mkdir $out/specialisation`, which fails once this aspect has created the
  # directory. Order between two `types.lines` contributions is not something to
  # rely on either way, so refuse the overlap with a readable message instead of
  # a mkdir error deep in a builder log.
  noOverlapAssertion = config: {
    assertion = config.specialisation or { } == { };
    message =
      "den specialisations and the hand-written `specialisation.<name>.configuration` option"
      + " cannot both be used on one configuration: ${
         lib.concatStringsSep ", " (lib.attrNames (config.specialisation or { }))
       } would clash with ${lib.concatStringsSep ", " (lib.attrNames config.denSpecialisations)}."
      + " Express the remaining ones as aspect compositions.";
  };

  routeTo =
    entity:
    den.lib.policy.route {
      fromClass = "specialisations";
      intoClass = entity.class;
      path = [ "denSpecialisations" ];
    };
in
{
  den.classes.specialisations.description = "Named specialisations, composed from aspects";

  # Routed by POLICIES, not forward-aspects: a forward only reaches the scope tree
  # it is included in, so `specialisations` written by a *user*-included aspect
  # (kanata, sketchybar, paneru -- precisely the ones a lockdown profile has to
  # reach) would be silently dropped. A policy is replicated into every
  # host-bound scope, user scopes included. Same reasoning, and the same shape,
  # as the `nix` class in aspects/base/nix.nix.
  den.policies.specialisations-to-host =
    { host, ... }:
    lib.optional (
      host ? class
      && builtins.elem host.class [
        "nixos"
        "darwin"
      ]
    ) (routeTo host);

  den.policies.specialisations-to-home =
    { home, ... }:
    lib.optional (home ? class) (routeTo home);

  den.aspects.specialisations = { host, ... }: {
    # NixOS builds the child itself: `inheritParentConfig = false` evaluates it
    # over `noUserModules` (nixpkgs nixos/lib/eval-config.nix), i.e. the base
    # modules plus ours and nothing of the parent's -- exactly the fresh eval a
    # composed aspect set wants. Writing our own `system.systemBuilderCommands`
    # here would collide with nixpkgs' unconditional `mkdir $out/specialisation`.
    nixos = { config, ... }: {
      options.denSpecialisations = contributionsOption;
      config = lib.mkIf (active host config) {
        # `.modules` rather than `.package`: NixOS wants the module list, and
        # taking its own option keeps the bootloader entries, `mkdir
        # $out/specialisation` and `switch-to-configuration --specialisation`
        # exactly as upstream writes them. Reading the list does not evaluate
        # the specialised system twice -- it is the argument den passed to
        # `nixosSystem`, not the result.
        specialisation = lib.mapAttrs (_: spec: {
          inheritParentConfig = false;
          configuration.imports = spec.modules;
        }) (builtByName "host" host config);
      };
    };

    # nix-darwin has no `noUserModules`, and the in-tree patch
    # (patches/darwin/specialisation.patch) is `extendModules`-only, so there is
    # no from-scratch child to ask for. Instantiate the composed system the way
    # den instantiates a host and link its toplevel into the same place the
    # patched module would have -- the on-disk layout is what activation reads,
    # so `/run/current-system/specialisation/<name>/activate` is unchanged.
    darwin = { config, ... }: {
      options.denSpecialisations = contributionsOption;
      config = lib.mkIf (active host config) (
        let
          built = builtByName "host" host config;
        in
        {
          assertions = [ (noOverlapAssertion config) ];
          system.systemBuilderCommands = linkCommands built;
          # The same handle NixOS gives you via
          # `specialisation.<n>.configuration.…`: `system.build` is
          # nix-darwin's escape hatch for exactly this, and eval-only checks
          # (or a quick `nix eval`) need to reach inside a specialisation
          # without building it.
          system.build.specialisations = lib.mapAttrs (_: spec: spec.configuration) built;
        }
      );
    };
  };

  # Standalone homes (`den.homes.<system>.<name>`). home-manager's own
  # specialisation module is `extendModules`-only too (modules/misc/specialisation.nix),
  # so the composed home is instantiated here and its activation package linked
  # into `$out/specialisation/<name>` -- the path `home-manager generations` +
  # `.../specialisation/<name>/activate` expects.
  den.aspects.home-specialisations = { home, ... }: {
    homeManager = { config, ... }: {
      options.denSpecialisations = contributionsOption;
      config = lib.mkIf (active home config) {
        assertions = [ (noOverlapAssertion config) ];
        home.extraBuilderCommands = linkCommands (builtByName "home" home config);
      };
    };
  };

  den.default.includes = [
    den.policies.specialisations-to-host
    den.policies.specialisations-to-home
  ];

  # Entity scope only: at default scope these would also resolve in every user
  # scope and route their class content back up, instantiating each
  # specialisation once per user.
  den.schema.host.includes = [ den.aspects.specialisations ];
  den.schema.home.includes = [ den.aspects.home-specialisations ];
}
