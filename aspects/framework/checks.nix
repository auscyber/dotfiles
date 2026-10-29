{
  lib,
  config,
  den,
  self,
  ...
}:
# ── A CHECK IS A LIST OF FINDINGS, AND THE DERIVATION IS PLUMBING ──
#
# `nix flake check` wants a derivation per check; a check wants to say "these
# four things are wrong, here is what to do". So the aspect that OWNS the fact
# writes the findings and nothing else:
#
#   den.checks.socket-exclusions = {
#     description = "No two givers claim one exclusive resource.";
#     findings = lib.concatMap den.lib.capability.exclusionsIn checkLib.hostEntities;
#     fix = "...";
#   };
#
# and this file turns every one of them into `checks.<name>`. Same split as
# ../base/firewall.nix, where each service declares its own `expose` and one
# derivation downstream opens the ports -- a check written here ABOUT another
# file would be the same fact in two places, able to disagree.
#
# ── WHY FINDINGS AND NOT A BOOLEAN ──
#
# ../../CLAUDE.md's standard is that the message says what is wrong, where, and
# what to do. A boolean cannot, and a `throw` reports only the first problem.
# A list reports every one of them together, which is the same reason
# ./requires.nix collects checks instead of asserting, and the same reason
# ../base/firewall.nix emits `assertions` rather than throwing on the first
# clash.
#
# ── AND WHY THEY ARE COMPUTED AT FLAKE LEVEL ──
#
# ../../CLAUDE.md: a full `nixosConfigurations.<host>.config` eval needs several
# GB and they must be serialised, so a check that evaluates every host is worse
# than no check. Every finding below is computed from ENTITIES and givers --
# `hasAspect` is a projected lookup and `gives` is a plain function, neither
# needs a host's fixpoint -- so the whole set answers in seconds. The checks
# that genuinely need rendered config are NOT here: they are `assertions` and
# `warnings` on the host that already pays for its own evaluation
# (../hosts/fleet.nix, ../base/firewall.nix, ../security/age-scope.nix).
#
# `nix eval .#checkFindings` is the view that needs no builder at all.
let
  inherit (lib) mkOption types;

  checkType = types.submodule {
    options = {
      description = mkOption {
        type = types.str;
        description = "What this check verifies, in one line. It appears in the output whether or not anything is wrong.";
      };

      findings = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = ''
          One string per thing that is wrong, saying WHAT and WHERE. `[ ]` is a
          pass.

          Kept lazy on purpose: nothing forces this until the check's derivation
          is instantiated or `flake.checkFindings` is read, so declaring a check
          costs a host evaluation nothing.
        '';
      };

      fix = mkOption {
        type = types.str;
        default = "";
        description = "What to do about any of them. Printed once rather than repeated per finding.";
      };

      severity = mkOption {
        type = types.enum [
          "error"
          "report"
        ];
        default = "error";
        description = ''
          `error` fails the check derivation, which is what `nix flake check`
          reports.

          `report` prints the findings and succeeds. For a check that is CORRECT
          and currently fails: a pre-existing bug the check just found should be
          visible to everybody without stopping everybody's builds, and silently
          dropping the check would lose the finding. Promote it to `error` once
          the finding is gone.
        '';
      };
    };
  };

  # ── THE FLEET, FLAT ──
  # `den.hosts` is keyed by system first, and every check here asks a question
  # about hosts rather than about systems. Four files were already carrying this
  # same line (./requires.nix, ../base/firewall.nix, ../base/service-reach.nix,
  # ../base/capability.nix); the checks read it from here instead.
  hostsOf = lib.concatMapAttrs (_: hs: hs) den.hosts;
  hostEntities = lib.attrValues hostsOf;

  # THE REAL ENTITY, NOT THE ONE A CLASS BODY IS HANDED. A class body's `host`
  # carries den's PROJECTED `hasAspect`, which answers "is this delivered into
  # THIS scope" rather than "does that host have it" -- the trap that left every
  # `expose` in the tree inert until ../base/firewall.nix was rewritten, and
  # that ../services/shares.nix hit before it. Any check written as an
  # assertion or a warning inside a host's config needs this.
  entityFor = host: hostsOf.${host.name or ""} or host;

  # ── A FINDING THAT IS ITSELF AN EVALUATION ERROR ──
  #
  # Every check here walks the WHOLE fleet, so one broken host would otherwise
  # take down the check for all of them -- which is how a half-finished edit to
  # one host's aspect turns a fleet-wide report into a single opaque trace. The
  # throw is reported as a finding instead, and because `builtins.tryEval`
  # discards the message the finding carries the command that reproduces it.
  #
  # Same reasoning as ../base/service-reach.nix's `try`, which swallows this
  # case for `buildConditions`; the difference is that a check has somewhere to
  # put it.
  probe =
    label: query: v:
    let
      r = builtins.tryEval v;
    in
    if r.success then
      r.value
    else
      [
        "${label}: `${query}` threw, so this could not be checked. Run it on its own to see why -- ../../CLAUDE.md's capability loop (`nix eval --impure .#debug.config --apply`) answers it in seconds, and a broken aspect on one host is the usual cause."
      ];

  # ── THE SAME DERIVATION, FOR A CHECK THAT CANNOT GO IN `den.checks` ──
  #
  # `flake.checkFindings` maps over EVERY declared check, so a single expensive
  # one makes the whole cheap view expensive. Measured: forcing
  # `config.patchedInputs` costs 2m10s on its own (it resolves every layer
  # lock), against 19s for the entire fleet-wide set. ../tooling/patch-inputs.nix
  # therefore builds its check directly with this, and `nix flake check` -- which
  # already pays for a `perSystem` evaluation -- is the only thing that runs it.
  #
  # Exported rather than inlined there so the OUTPUT is identical whichever route
  # a check takes.
  mkCheck =
    pkgs: name: raw:
    let
      c = {
        fix = "";
        severity = "error";
      }
      // raw;
    in
    let
      report = pkgs.writeText "${name}-findings" (
        lib.concatStringsSep "\n" (
          [
            "${name}: ${toString (lib.length c.findings)} finding(s) -- ${c.description}"
            ""
          ]
          ++ map (f: "  - ${f}") c.findings
          ++ lib.optionals (c.fix != "") [
            ""
            "  fix: ${c.fix}"
          ]
          ++ [ "" ]
        )
      );
    in
    if c.findings == [ ] then
      pkgs.runCommand name { passthru.findings = [ ]; } ''
        cp ${pkgs.writeText "${name}-ok" "${name}: ok -- ${c.description}\n"} "$out"
      ''
    # REPORT: the findings are in `$out` as well as on stderr, because a
    # succeeding builder's stderr is only shown under `-L` and the point of a
    # report is that somebody can read it.
    else if c.severity == "report" then
      pkgs.runCommand name { passthru.findings = c.findings; } ''
        cat ${report} >&2
        cp ${report} "$out"
      ''
    else
      pkgs.runCommand name { passthru.findings = c.findings; } ''
        cat ${report} >&2
        exit 1
      '';

  # ── WHICH TAKER OPTIONS NOBODY READS ──
  #
  # A capability declares taker options, aspects set them, the resolver returns
  # them -- and if no giver folds them in, the feature is simply absent while
  # every file looks correct. That has happened twice here: four aspects
  # contributed `takes.home-assistant.extraComponents` that nothing read, and
  # then `contributions` was found to ask `takersOn host` (takers on the giver's
  # OWN box) so ../services/openrgb.nix's component reached nobody.
  #
  # ── WHY THE READER SIDE IS A GREP AND NOT AN EVALUATION ──
  #
  # "Does some giver read this" is not a fact the entity graph holds: a reader is
  # a call in a `nixos` body, and reaching it means evaluating that body, which
  # is the host evaluation this whole file exists to avoid. So the eval half
  # produces the question -- which taker options are SET, and by whom -- and the
  # builder greps the tree for an answer, which is cheap and needs no host.
  #
  # The alternative is for a giver to DECLARE that it reads them, which is the
  # better answer and belongs in the capability definition rather than here; see
  # this file's report.
  #
  # ── PER KEY, BECAUSE THERE ARE TWO WAYS TO READ ONE ──
  #
  # Measured, as the check's first false positive: `endpoint`'s own
  # `direction` is read by `take.direction or "in"` inside ANOTHER capability's
  # `rules`, so nothing names `"endpoint"` anywhere and the capability-level
  # question answered "nobody". A key read straight off a take is a reader, and
  # it is a reader of that KEY rather than of the capability -- which is also
  # the honest unit: a capability with four taker options and one reader that
  # folds in two of them is three-quarters wired up.
  takerOptionSpec =
    let
      settersOf =
        cap: _:
        let
          perTaker =
            n: aspect:
            let
              k = den.lib.capability.takeKeyFor cap aspect;
              # RAW take: it carries only the keys the taker actually SET, which
              # is the question here -- a default that materialises through
              # `contributions` is not a contribution anybody made.
              # ../../CLAUDE.md's second trap, and `or` is the correct operator
              # on a raw take for exactly that reason.
              take = (den.lib.capability.takesOf aspect).${k} or { };
            in
            map (key: {
              inherit key;
              taker = n;
            }) (lib.filter (key: take ? ${key}) (den.lib.capability.takerNames cap));

          flat = lib.concatLists (lib.mapAttrsToList perTaker (den.lib.capability.takers cap));
        in
        lib.mapAttrs (_: es: map (e: e.taker) es) (lib.groupBy (e: e.key) flat);
    in
    lib.concatLists (
      lib.mapAttrsToList
        (
          cap: byKey:
          lib.mapAttrsToList (key: takers: "${cap} ${key} ${lib.concatStringsSep "," takers}") byKey
        )
        (
          lib.mapAttrs settersOf (
            lib.filterAttrs (cap: _: den.lib.capability.takerNames cap != [ ]) config.den.capabilities
          )
        )
    );
in
{
  options.den.checks = mkOption {
    type = types.attrsOf checkType;
    default = { };
    description = ''
      Checks `nix flake check` runs, declared by the aspect that owns the fact
      being checked. Keyed by check name, which is also the name of the
      resulting `checks.<name>` derivation.
    '';
  };

  # ── THE LIB SCOPE EVERY CHECK FILE READS ──
  #
  # Not an `import`: ../../CLAUDE.md -- a path import is re-evaluated per call
  # site and hard-codes the importer's position relative to this file. Same
  # route ../base/capability.nix takes for `capability`.
  config._module.args.checkLib = {
    inherit
      hostsOf
      hostEntities
      entityFor
      probe
      mkCheck
      ;
  };

  # The whole set without building anything, which is the loop to develop a
  # check in: `nix eval --json .#checkFindings`.
  config.flake.checkFindings = lib.mapAttrs (_: c: {
    inherit (c) description severity findings;
  }) config.den.checks;

  config.perSystem =
    { pkgs, ... }:
    {
      checks = lib.mapAttrs (mkCheck pkgs) config.den.checks // {

        # ── PARKED HERE: belongs in ../base/capability.nix ──
        # It is a rule of the capability layer ("a declared taker option must
        # have a reader"), and the layer is where the question is answerable
        # without a grep. Written here only because that file is being edited
        # elsewhere.
        capability-contributions-read =
          let
            spec = pkgs.writeText "capability-taker-options" (lib.concatStringsSep "\n" takerOptionSpec + "\n");
          in
          pkgs.runCommand "capability-contributions-read"
            {
              nativeBuildInputs = [
                pkgs.gnugrep
                pkgs.findutils
              ];
              inherit spec;
              src = self;
            }
            ''
              fail=0

              # ── A COMMENT IS NOT A READER ──
              # Found by a deliberate break: renaming ../services/shares.nix's
              # two `takersOn u "share"` calls left the check GREEN, because
              # that file's own header quotes the call it is describing
              # (`"takersOn host \"share\"` comes back EMPTY"). So every match
              # whose line begins with `#` is dropped. The repo's style is a
              # comment per decision, so this is not a corner case here.
              reader() {
                grep -rE --include='*.nix' "$1" "$src/aspects" \
                  | grep -vE '^[^:]*:[[:space:]]*#' \
                  | grep -v 'aspects/base/capability\.nix:' \
                  >/dev/null
              }

              while read -r cap key setters; do
                [ -n "$cap" ] || continue

                # ONE: A GIVER FOLDS THE WHOLE CAPABILITY IN. Any of the four
                # routes a giver actually takes --
                # `contributions`/`contributionsReaching` are the typed fold,
                # `takersOn`/`takersReaching` the raw walk that
                # ../services/kanidm.nix and ../services/mail.nix use to read a
                # take's keys themselves. ../base/capability.nix is excluded: it
                # implements all four and would answer for every capability.
                if reader "(contributions|contributionsReaching|takersOn|takersReaching)[^\"]*\"$cap\""; then
                  continue
                fi

                # TWO: SOMETHING READS THE KEY OFF A TAKE DIRECTLY, in a file
                # that knows which capability it is talking about. A
                # capability's own `rules` do this -- `endpoint`'s `direction`
                # is read as `take.direction or "in"` and named nowhere -- and
                # the rules live beside the definition, in a file that spells
                # the capability's name.
                #
                # SCOPED, because the key name alone is not unique: `share` and
                # `oidc` both declare `displayName`, and an unscoped match let
                # ../services/kanidm.nix answer for the share capability.
                if grep -rlE --include='*.nix' "capabilities\.$cap\b|\"$cap\"" "$src/aspects" \
                     | xargs -r grep -lE "take\.$key\b|take \? $key\b" \
                     >/dev/null 2>&1; then
                  continue
                fi

                fail=1
                echo "FAIL $cap.$key: set by $setters, and nothing reads it." >&2
                echo "      No giver folds \"$cap\" in (contributions/contributionsReaching/takersOn/takersReaching)" >&2
                echo "      and nothing that names \"$cap\" reads \`take.$key\`, so every value above is" >&2
                echo "      collected and discarded -- the feature is absent while every file looks correct." >&2
                echo "      Fix: fold it in from the giver (den.lib.capability.contributionsReaching den.aspects.<giver> \"$cap\" host)," >&2
                echo "      or drop taker.options.$key so nothing can set it." >&2
              done < "$spec"

              if [ "$fail" -ne 0 ]; then
                echo "" >&2
                echo "capability-contributions-read: a taker option somebody set must have a reader." >&2
                exit 1
              fi

              echo "capability-contributions-read: ok" > "$out"
            '';
      };
    };
}
