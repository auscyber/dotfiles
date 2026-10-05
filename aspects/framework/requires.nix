{
  den,
  lib,
  config,
  ...
}:
# `requires` -- what an aspect needs to be true, as a script that says so.
#
#   den.aspects.celler-host.requires = { host, ... }: {
#     celler = {
#       description = "a celler answers";
#       script = "curl -fsS --max-time 5 ${url} >/dev/null";
#     };
#   };
#
# The check belongs to the thing that NEEDS it. `celler-host` is what breaks
# when no celler answers, so `celler-host` is what says so -- not a profile
# somewhere else naming it, which would be the same fact written twice and able
# to disagree.
#
# ── PARAMETRIC, LIKE `gives` ──
#
# An aspect does not know which host it is on, and a reachability check is
# exactly the thing that depends on it: the laptop reaches a celler over the
# tailnet and secondpc over loopback. So `requires` takes the scope context, the
# same shape ../base/capability-types.nix gives `gives`, and `den.lib.canTake`
# gates it so an aspect asking for an arg the scope has not got is inert rather
# than throwing from inside someone else's file.
#
# ── WHAT CONSUMES IT ──
#
# ../tooling/nh-checked.nix runs these before a build and, when some fail, picks
# the specialisation that excludes the aspects that failed -- see
# `flake.buildConditions` below, which is the pure, host-eval-free view it reads.
#
# A `requires` is deliberately NOT an assertion. An aspect whose peer is down is
# not misconfigured; it is a machine that should come up without that aspect,
# which is what a specialisation already expresses.
let
  inherit (lib) mkOption types;

  # The checks one aspect declares, in one scope. Null when the aspect declares
  # none, or when the scope cannot supply the args it asked for.
  requiresOf =
    ctx: aspect:
    let
      r = aspect.requires or null;
      args = ctx // {
        self = aspect;
      };
    in
    if r == null then
      { }
    else if lib.isFunction r then
      (if den.lib.canTake.atLeast args r then r args else { })
    else
      r;

  internal =
    n:
    lib.hasPrefix "_" n
    || builtins.elem n [
      "provides"
      "includes"
      "excludes"
      "meta"
      "name"
      "policies"
      "classes"
      "description"
    ];

  # Picked out BY NAME rather than passed through: den wraps nested content, so
  # a check can carry `_` (a functor) and `__aspectChain` alongside the real
  # keys, and handing those to `builtins.toJSON` is "cannot convert a function
  # to JSON". Naming them also makes a typo an eval error here rather than a
  # silently absent check at build time.
  checkOf = check: {
    description = check.description or "";
    script = check.script;
  };

  hostsOf = lib.concatMapAttrs (_: hs: hs) den.hosts;

  # `<aspect> = { <check> = { description; script; }; }` for one host. The same
  # view the flake output publishes and the runtime command below runs, so the
  # two cannot answer differently.
  #
  # ── DECLARED AND DERIVED, MERGED HERE AND NOWHERE ELSE ──
  #
  # An aspect writes `requires` when the thing it needs is particular -- a
  # celler answering `nix-cache-info` is not a question anything could infer.
  # A take that crosses machines is the opposite: the declaration already says
  # what is needed and ../base/service-reach.nix turns it into the same shape.
  #
  # `//` per aspect rather than `recursiveUpdate` over the whole map: two
  # sources for one aspect merge their CHECKS, and a derived check cannot
  # replace a hand-written one unless it is named the same -- in which case the
  # hand-written one wins, because the file that names a service by hand knows
  # something this does not.
  requiresFor =
    host:
    let
      derived = config.den.derivedRequires.${host.name} or { };

      declared = lib.mapAttrs (
        _: aspect:
        lib.mapAttrs (_: checkOf) (
          lib.filterAttrs (n: _: !internal n) (requiresOf { inherit host; } aspect)
        )
      ) (lib.filterAttrs (_: a: (a.requires or null) != null && host.hasAspect a) den.aspects);
    in
    lib.filterAttrs (_: v: v != { }) (
      lib.genAttrs (lib.unique (lib.attrNames declared ++ lib.attrNames derived)) (
        n: (lib.mapAttrs (_: checkOf) (derived.${n} or { })) // (declared.${n} or { })
      )
    );
in
{
  # ── WHERE A DERIVED CHECK ARRIVES ──
  #
  # `<host> = { <aspect> = { <check> = { description; script; }; }; }`, written
  # by anything that can work out a requirement from a declaration already
  # made. ../base/service-reach.nix is the one that does it today, off
  # `takes`: a take that resolves to another machine is a dependency on that
  # machine being up, and it does not need saying twice.
  #
  # An OPTION rather than those files writing `den.aspects.<n>.requires`
  # directly: `requires` is `types.raw` with no merge function, so a second
  # definition of one aspect's is "already defined" rather than two checks --
  # and the aspect being described is often not the one being derived FROM.
  options.den.derivedRequires = mkOption {
    type = types.attrsOf (types.attrsOf (types.attrsOf types.raw));
    default = { };
    description = ''
      Checks worked out from a declaration rather than written by hand, keyed
      by host, then by the ASPECT that breaks if the check fails -- which is
      what ../tooling/nh-checked.nix matches a specialisation's `excludes`
      against.
    '';
  };

  config.den.schema.aspect = {
    options.requires = mkOption {
      type = types.nullOr types.raw;
      default = null;
      description = ''
        What this aspect needs to be true, keyed by check name, each
        `{ description; script; }`. Takes the scope context -- `{ host, self, ... }`
        -- so a check can resolve per host, the way `gives` does.

        A FAILING script (non-zero exit) means the aspect cannot work here.
        ../tooling/nh-checked.nix reads these and selects a specialisation that
        excludes whatever failed.
      '';
    };
  };

  # ── THE PURE VIEW THE WRAPPER READS ──
  #
  # Per host: the checks every aspect it has declares, and the exclude set of
  # every specialisation declared for it. The wrapper runs the first and matches
  # against the second.
  #
  # READ OFF THE ASPECT TREE, NOT A HOST CONFIG. The routed class content is the
  # authoritative set for specialisations, but it lives on the host's module
  # system, so reading it costs a full host evaluation -- which a wrapper that
  # runs BEFORE every build cannot pay. These come from `den.aspects`, which is
  # declared content and evaluates in seconds.
  #
  # THE LIMITATION THAT FOLLOWS, stated rather than hidden: only aspects
  # reachable at flake level are read, so a specialisation routed in from a
  # user-scoped contribution is not seen here. `excludes` written on the host's
  # own aspect -- which is where they are written -- is.
  # ── THE SAME CHECKS, ON THE MACHINE ──
  #
  # `requires-check` runs this host's own requires and says what is true right
  # now. The build-time wrapper answers "what should I build"; this answers "why
  # is this box behaving like that", which is the question you have once it is
  # already running -- a celler-push unit retrying against a server that went
  # away looks like a broken unit until something tells you the peer is gone.
  #
  # Same scripts, same toolbox as ../tooling/nh-checked.nix, so a check cannot
  # pass in one place and fail in the other for reasons of environment.
  #
  # `--json` so this can be read by something else later -- `specialise` picking
  # a profile on the box is the obvious next consumer -- rather than only by a
  # person.
  config.den.aspects.requires-check.os =
    {
      pkgs,
      host ? { },
      ...
    }:
    let
      checks = requiresFor host;

      lines = lib.concatLists (
        lib.mapAttrsToList (
          aspect:
          lib.mapAttrsToList (
            check: c: {
              inherit aspect check;
              inherit (c) description script;
            }
          )
        ) checks
      );

      runOne = c: ''
                if ${pkgs.bash}/bin/bash -c ${lib.escapeShellArg c.script} >/dev/null 2>"$err"; then
                  printf 'ok    %s %s
        ' ${lib.escapeShellArg c.aspect} ${lib.escapeShellArg c.check}
                else
                  failed=1
                  printf 'FAIL  %s %s -- %s
        ' ${lib.escapeShellArg c.aspect} ${lib.escapeShellArg c.check}             ${lib.escapeShellArg c.description}
                  ${pkgs.gnused}/bin/sed 's/^/        /' "$err"
                fi
      '';
    in
    {
      environment.systemPackages = lib.optional (lines != [ ]) (
        pkgs.writeShellApplication {
          name = "requires-check";
          runtimeInputs = [
            pkgs.coreutils
            pkgs.curl
            pkgs.openssh
            pkgs.netcat
          ];
          text = ''
                        if [ "''${1:-}" = "--json" ]; then
                          printf '%s
            ' ${lib.escapeShellArg (builtins.toJSON checks)}
                          exit 0
                        fi

                        failed=0
                        err="$(mktemp)"
                        trap 'rm -f "$err"' EXIT

                        ${lib.concatStringsSep "
" (map runOne lines)}

                        exit "$failed"
          '';
        }
      );
    };

  config.den.schema.host.includes = [ den.aspects.requires-check ];

  config.flake.buildConditions =
    let
      forHost =
        host:
        let
          mine = lib.filterAttrs (_: a: host.hasAspect a) den.aspects;

          requires = requiresFor host;

          # `<spec> = [ <excluded aspect names> ]`. The names are what the
          # wrapper matches failed aspects against, so they are read off the
          # same `name` den identifies an aspect by.
          profilesOf =
            aspect:
            let
              specs = aspect.specialisations or { };
            in
            if !(builtins.isAttrs specs) then
              { }
            else
              lib.mapAttrs (_: spec: map (e: e.name or "<anon>") (lib.flatten (spec.excludes or [ ]))) (
                lib.filterAttrs (n: _: !internal n) specs
              );

          profiles = lib.foldl' lib.recursiveUpdate { } (
            lib.mapAttrsToList (_: profilesOf) (
              lib.filterAttrs (_: a: (a.specialisations or null) != null) mine
            )
          );
        in
        lib.optionalAttrs (requires != { }) { inherit requires profiles; };
    in
    lib.filterAttrs (_: v: v != { }) (lib.mapAttrs (_: forHost) hostsOf);
}
