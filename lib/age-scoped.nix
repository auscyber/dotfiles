# Shared schema for `age.scoped` -- per-scope groups of agenix secrets/templates.
#
#   age.scoped.<scope> = {
#     service   = "slskd";          # defaults to <scope>
#     settings  = { owner = "music"; };
#     secrets."env"    = { generator = ...; };
#     templates.config = { content = ...; };
#   };
#
# Every entry is flattened into the ordinary `age.secrets` / `age.templates`
# namespaces under `<scope><separator><key>`, so nothing downstream of agenix has
# to know scopes exist. `access` maps the short keys back onto the flattened
# entries, for referring to a scope's own secrets without spelling the prefix.
#
# Per-entry values win over the scope's `settings`, which win over the defaults
# `backend` infers from `service` -- so a secret can always opt out of whatever
# was inferred for it.
#
# `class` selects the per-class half: given the resolved `service` name, the
# backend returns the `owner`/`group`/`restartUnits` defaults for that module
# system (systemd on NixOS, launchd on darwin, user units under home-manager).
#
# Those lookups are the delicate part. `config.age.secrets` ends up defined in
# terms of `config.services.<s>` while services define
# `environmentFile = config.age.secrets.<k>.path`, so a backend may only touch
# cheap leaves (`.enable`, `.user`) reached through `or`, and must never force
# the attribute names of `systemd.services`. A scope named after an aspect rather
# than a service (`secondpc-web`, `celler-push`, ...) must yield `{ }`, not an
# error. If a host ever wedges on infinite recursion anyway, the escape hatch is
# `age.scoped.<s>.service = null` plus an explicit `settings.restartUnits`.
#
# Lives outside `aspects/` (dendritic auto-import) and outside `extraModules/`
# (whose top-level dirs are read as *class names* by aspects/tooling/extraModules.nix),
# so this file is only ever pulled in by an explicit `import` -- same as
# lib/tmpfiles.nix.
{
  lib,
  class,
}:
{
  config,
  pkgs,
  ...
}:
let
  inherit (lib) mkOption types;

  # The service's declared config, or null when the scope is not a service at
  # all. `config.services` is a plain attrset of evaluated options, so `or` is
  # safe on an undeclared name.
  serviceCfg = service: config.services.${service} or null;

  enabled =
    service:
    let
      svc = serviceCfg service;
    in
    svc != null && (svc.enable or false);

  # `services.<s>.user` where the service module declares one. Some modules
  # default it to a non-string (null, an integer uid) -- only a string is a
  # usable agenix `owner`.
  serviceOwner =
    service:
    let
      user = (serviceCfg service).user or null;
    in
    if builtins.isString user && user != "" then user else null;

  # `owner` plus the matching primary group, when the user is actually declared.
  # A service running as a user it does not declare (`owner = "1000"`) still
  # gets the owner, just no group.
  ownership =
    service:
    let
      owner = serviceOwner service;
    in
    lib.optionalAttrs (owner != null) (
      {
        inherit owner;
      }
      // lib.optionalAttrs (config.users.users ? ${owner}) {
        inherit (config.users.users.${owner}) group;
      }
    );

  backends = {
    nixos = service: { restartUnits = [ "${service}.service" ]; } // ownership service;

    # nix-darwin's activation runs `launchctl kickstart -k "system/$unit"` with
    # no `|| true`, so an unknown label would fail activation outright -- only
    # emit `restartUnits` for a daemon that demonstrably exists.
    darwin =
      service:
      lib.optionalAttrs (config.launchd.daemons ? ${service}) {
        restartUnits = [
          (config.launchd.daemons.${service}.serviceConfig.Label or "org.nixos.${service}")
        ];
      }
      // ownership service;

    # home-manager's agenix module declares neither `owner` nor `group` (the
    # secrets are the user's by construction), so this contributes restarts only.
    homeManager =
      service:
      if pkgs.stdenv.hostPlatform.isDarwin then
        lib.optionalAttrs (config.launchd.agents ? ${service}) {
          restartUnits = [
            (config.launchd.agents.${service}.config.Label or "org.nix-community.home.${service}")
          ];
        }
      else
        lib.optionalAttrs (config.systemd.user.services ? ${service}) {
          restartUnits = [ "${service}.service" ];
        };
  };

  backend = service: lib.optionalAttrs (enabled service) (backends.${class} service);

  cfg = config.age.scoped;

  # The name a scoped entry takes in the flat namespace.
  flatName = scope: key: "${scope.name}${scope.separator}${key}";

  # The per-aspect view of a scope: the scope itself, but with `secrets` and
  # `templates` swapped from the raw declarations to the DEPLOYED entries under
  # their short names. So `scoped.secrets.celler_token` is the thing you pass to
  # a dependency, and `scoped.name` / `scoped.access` / `scoped.settings` still
  # read as they do on the option.
  #
  # Handed to entry module functions below, and to whole-class `secrets` /
  # `templates` bodies by aspects/security/age-scope.nix, which also aliases
  # `secrets` = `scoped.secrets` and `templates` = `scoped.templates`.
  scopeView =
    scope:
    scope
    // {
      secrets = builtins.intersectAttrs scope.secrets scope.access;
      templates = builtins.intersectAttrs scope.templates scope.access;
    };

  scopeLocalArgs =
    scope:
    let
      view = scopeView scope;
    in
    {
      scoped = view;
      inherit (view) secrets templates;
    };

  # An entry is a module, not a plain attrset: `age.secrets.<n>` is a submodule
  # upstream, so `templates.env = { secrets, ... }: { ... }` has always been
  # legal and has to stay legal here. A freeform submodule accepts both shapes
  # and merges several definitions of one entry properly -- which is what lets
  # aspects/network/vpn.nix add `owner`/`group` to a `wireguard_key` that
  # aspects/network/vpn.nix's own `vpn-secrets` aspect declared elsewhere.
  entryType =
    scope:
    types.submodule {
      # `types.attrs` (shallow `//` across definitions), NOT `lazyAttrsOf raw`.
      # An aspect can legitimately arrive twice -- `user-pwd` is included both
      # directly and through `auscyber.provides.to-hosts` -- and `raw` rejects a
      # second definition even when it is identical, so every field of every
      # secret in such an aspect became "defined multiple times". `types.anything`
      # is not the fix either: it would compare the duplicated `content` /
      # `generator.script` FUNCTIONS for equality and fail on those instead.
      #
      # Shallow merge is also what the pre-scoping `attrsOf types.attrs` did, so
      # two files adding different fields to one secret keep working exactly as
      # they did (aspects/network/vpn.nix adds `owner`/`group` to a `wireguard_key`
      # declared elsewhere).
      freeformType = types.attrs;
      config._module.args = scopeLocalArgs scope;
    };

  scopeType = types.submodule (submod: {
    options = {
      name = mkOption {
        type = types.str;
        default = submod.config._module.args.name;
        defaultText = "‹scope›";
        description = ''
          Scope name, used as the prefix on every entry. Defaults to the
          attribute name, which for aspect-derived scopes is the aspect name.
        '';
      };

      service = mkOption {
        type = types.nullOr types.str;
        default = submod.config.name;
        defaultText = "‹scope›";
        description = ''
          The service this scope belongs to, used to infer `owner`, `group` and
          `restartUnits` for every entry. Harmless when no such service exists
          -- the backend guards its lookups and contributes nothing. Set to
          `null` to disable inference entirely.
        '';
      };

      separator = mkOption {
        type = types.str;
        default = "/";
        description = ''
          Joins the scope name to each key. `/` nests the deployed secrets under
          a per-scope directory (`/run/agenix/slskd/env`), which is why agenix-rekey
          needs patches/agenix-rekey/rekey-mkdir-p.patch. Set to `-` for a scope
          whose secrets must stay in a flat directory.
        '';
      };

      secrets = mkOption {
        type = types.attrsOf (entryType submod.config);
        default = { };
        description = ''
          Secrets in this scope, merged into `age.secrets.<scope><sep><key>`.
          Each entry may be an attrset or a module function; a function receives
          `secrets` and `templates` rebound to THIS scope, keyed by short name.
        '';
      };

      templates = mkOption {
        type = types.attrsOf (entryType submod.config);
        default = { };
        description = ''
          Templates in this scope, merged into `age.templates.<scope><sep><key>`.
          Each one implicitly depends on every secret in the scope, so a template
          only has to name dependencies from *outside* its scope. Same
          attrset-or-module-function shape as `secrets`.
        '';
      };

      settings = mkOption {
        type = types.attrs;
        default = { };
        description = ''
          Defaults applied to every secret and template in this scope. Overrides
          whatever `service` inference produced; individual entries override this.
        '';
      };

      access = mkOption {
        readOnly = true;
        type = types.attrsOf types.attrs;
        description = ''
          This scope's entries under their short keys, pointing at the flattened
          `age.secrets` / `age.templates` definitions. Lets a scope refer to its
          own secrets (`scoped.slskd.access.env.path`) without repeating the prefix.
        '';
        default =
          lib.mapAttrs (key: _: config.age.secrets.${flatName submod.config key}) submod.config.secrets
          // lib.mapAttrs (
            key: _: config.age.templates.${flatName submod.config key}
          ) submod.config.templates;
      };
    };
  });

  inferred = scope: if scope.service == null then { } else backend scope.service;

  mkSecret = scope: value: inferred scope // scope.settings // value;

  mkTemplate =
    scope: value:
    let
      merged = inferred scope // scope.settings // value;
      # Every secret in the scope, keyed by its short name -- what the template's
      # `content` placeholders are written against.
      scopeDeps = lib.mapAttrs (key: _: config.age.secrets.${flatName scope key}) scope.secrets;
      explicit = value.dependencies or { };
    in
    merged
    // lib.optionalAttrs (builtins.isAttrs explicit) {
      dependencies = scopeDeps // explicit;
    };

  flatten =
    field: mk:
    lib.concatMapAttrs (
      _: scope:
      lib.mapAttrs' (key: value: lib.nameValuePair (flatName scope key) (mk scope value)) scope.${field}
    ) cfg;
in
{
  options.age.scoped = mkOption {
    type = types.attrsOf scopeType;
    default = { };
    description = ''
      Agenix secrets and templates grouped by scope. Aspects do not normally
      write this directly -- aspects/security/age-scope.nix nests each aspect's
      `secrets` / `templates` class content under the aspect's own name.
    '';
  };

  config = {
    age.secrets = flatten "secrets" mkSecret;
    age.templates = flatten "templates" mkTemplate;

    # Available in every module of this class, so a body can read
    # `secrets."slskd/env".path` or `scoped.slskd.secrets.env.path` rather than
    # reaching through `config.age.*`. `secrets`/`templates` are the flat global
    # sets and mean the same thing in every aspect -- another aspect's secrets
    # are reached exactly like your own.
    #
    # The `scoped` ARG is the viewed tree, not the raw option: under it,
    # `scoped.<name>.secrets` / `.templates` are the evaluated `age.secrets` /
    # `age.templates` entries keyed by short name, so they are directly usable as
    # dependencies and have `.path` / `.file`. The option `age.scoped.<name>.secrets`
    # keeps holding the declarations -- that is where you write them, this is how
    # you read them back.
    _module.args = {
      inherit (config.age) secrets templates;
      scoped = lib.mapAttrs (_: scopeView) cfg;
    };
  };
}
