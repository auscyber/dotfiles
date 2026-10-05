{
  den,
  lib,
  ...
}:
# A HOUSEHOLD IS A SCOPE HOSTS SIT IN, not a thing a host points at.
#
# Before this file there were two scope trees joined by a pointer:
#
#   flake -> flake-system -> host -> user
#   flake -> household -> member
#                      -> service
#
# `den.schema.service.parent = "household"` (./service.nix) makes a household a
# scope with child kinds already; hosts were not among them. A host merely
# CARRIED its household as a field, so every "the other machines in this house"
# question had to be hand-rolled from `den.hosts` -- and each hand-rolled copy
# got a different answer. ../base/capability.nix needed a `takersReaching`
# beside `takersOn` and a `contributionsReaching` beside `contributions`
# precisely because `takersOn host` could only see one host's own takes.
#
# den's own answer is the FLEET GROUPING PATTERN (den
# docs/explanation/fleet.md): two policies, one creating the grouping scope and
# one registering hosts as its children. Hosts under one grouping scope are
# SIBLINGS, which is the only thing `pipe.collect` looks at --
# `findMatchingSiblings` (den nix/lib/aspects/fx/assemble-pipes.nix) is
# `scopeParent.<a> == scopeParent.<b>` and nothing else.
#
# ── WHY `household` AND NOT `den.schema.fleet` ──
#
# den pre-registers `den.schema.fleet = { }` (den modules/options.nix) and this
# tree uses it nowhere, so it was the obvious candidate. It is the wrong one:
#
#   * `den.schema.fleet = { }` carries no options, and `isEntity` is COMPUTED
#     from whether the schema entry carries any (den modules/options.nix
#     `computed`). So `fleet` is not an entity kind -- den's own flake-parts
#     policy has to write `den.schema.flake-parts.isEntity = true` by hand for
#     the same reason. A fleet scope would therefore be a ROUTING kind.
#   * The grouping record would have to be synthetic -- `{ name = "ivymectin";
#     }` -- a second, identity-less copy of a household that already exists,
#     declared in ../households/, with rooms, members and services on it. Every
#     consumer would then have to map the fleet's name back to the real
#     household to ask it anything.
#   * `{ household, ... }` binds at a `household`-kind scope for free, because
#     ctx binding keys on the entity KIND name. Under a `fleet` scope it would
#     not, and `{ fleet, ... }` answers no question anyone here asks.
#   * `fleet` means the whole estate in this tree's own vocabulary
#     (../hosts/fleet.nix is every machine, including the ones with no
#     household). Spending the name on a per-house group would make a real
#     fleet scope -- above households -- unnameable.
#
# ── AND NOT `den.schema.host.parent = "household"` ──
#
# `parent` is the SCHEMA DAG, not the scope tree: it drives descendant
# entity-arg fan-out and `ancestorChain`, and creates no scope at all (den
# nix/lib/aspects/fx/arg-class.nix). Setting it would therefore not have made
# hosts siblings, and it would have changed two things that currently work:
#
#   * `ancestorChain "host"` becomes `[ host household ]`, and
#     policy/schema.nix's `ownerKind` picks the TOPMOST ancestor in ctx
#     carrying a `__pathSetByScope` -- so `host.hasAspect` on a householded
#     host would start reading the household's bucket instead of its own.
#   * `{ host, ... }` would fan out over `household.hosts`, an attribute that
#     does not exist and must not: a household pointing back at an entity that
#     resolves aspects is the direction violation ./person.nix's header records
#     as `infinite recursion encountered` in `systemd.services`.
#
# The grouping is PARTIAL, which is the one thing here that is not in den's
# doc. `member` and `service` always have a household, so both existing uses of
# `parent = "household"` are total. Hosts are not: contabo, celler2, lora-pi
# and the darwin machines have none and must keep `flake-system` as their
# parent. That is why den's own `system-to-os-outputs` is EXCLUDED and replaced
# by two policies rather than supplemented by one -- see the comment on the
# exclude below.
let
  inherit (den.lib.policy) resolve instantiate;

  # Every den host of a system. Read at FILE level, where the direction rule is
  # not at stake -- the same read ../hosts/fleet.nix and ../base/capability.nix
  # make, and the same one den's own `system-to-os-outputs` makes.
  hostsOn = system: builtins.attrValues (den.hosts.${system} or { });

  # `den.schema.host.options.household` is `nullOr raw` with `default = null`,
  # so the attribute EXISTS and `host.household or x` would never fire
  # (../../CLAUDE.md's first trap). Test it.
  householdOf = host: host.household;
  householdNameOf = host: if householdOf host == null then null else (householdOf host).name;

  # den's own policy gates on this and the replacement has to gate identically:
  # a host with an empty `intoAttr` produces no flake output, and
  # `specDescriptor` (den nix/lib/aspects/fx/edges/instantiate.nix) drops its
  # instantiate spec anyway.
  hasOutput = host: host.intoAttr != [ ];

  # The pair den's `system-to-os-outputs` emits per host, verbatim: create the
  # host's scope, then ask for its class content to be instantiated into
  # `flake.<intoAttr>`. Only WHERE it is emitted from changes.
  attachHost = host: [
    (resolve.to "host" { inherit host; })
    (instantiate host)
  ];

  # The households that have at least one host on this system, deduplicated by
  # name. Keyed off each host's own `household` field rather than
  # `config.den.households`, so this reads the pointer that already exists
  # instead of opening a second route to the same value -- and so a household
  # with no machines on a system gets no empty scope there.
  householdsOn =
    system:
    builtins.attrValues (
      builtins.listToAttrs (
        map (h: lib.nameValuePair (householdNameOf h) (householdOf h)) (
          builtins.filter (h: householdOf h != null) (hostsOn system)
        )
      )
    );
in
{
  config = {
    # ── POLICY 1: flake-system -> household ──
    #
    # One scope per (system, household). Per-system rather than one scope under
    # `flake` for all of them because `system` must stay in ctx for the hosts
    # below -- and because `pipe.collect` is defined on siblings, so a group
    # spanning systems would quietly make hosts of different systems peers,
    # which den reserves for `collectAll`/`broadcast`.
    den.policies.system-to-households =
      { system, ... }:
      map (household: resolve.to "household" { inherit household; }) (householdsOn system);

    # ── POLICY 2: household -> host ──
    #
    # `{ household, ... }` is an entity-kind arg, so den's late-dispatch filter
    # (den nix/lib/aspects/fx/policy/schema.nix `latePolicies`) fires this only
    # at scopes whose own kind is `household` -- it cannot re-fire at the host
    # scopes it creates. `system` comes from the flake-system ctx the household
    # scope inherits.
    #
    # It is also registered on `den.schema.household.includes`, which
    # `den.lib.resolveEntity` reads when a household instance computes its own
    # `config.resolved`. There is no `system` in that context, so
    # `resolveArgsSatisfied` (den nix/lib/aspects/fx/policy/dispatch.nix) skips
    # it and no host is attached twice. The gate is structural, not a flag.
    den.policies.household-to-hosts =
      { household, system, ... }:
      lib.concatMap attachHost (
        builtins.filter (h: hasOutput h && householdNameOf h == household.name) (hostsOn system)
      );

    # ── POLICY 3: flake-system -> host, for the hosts with no household ──
    #
    # The partial half. A host with no household keeps the parent it has today.
    den.policies.system-to-loose-hosts =
      { system, ... }:
      lib.concatMap attachHost (
        builtins.filter (h: hasOutput h && householdOf h == null) (hostsOn system)
      );

    den.schema.flake-system.includes = [
      den.policies.system-to-households
      den.policies.system-to-loose-hosts
    ];

    # ── WHY DEN'S OWN POLICY IS REPLACED RATHER THAN SUPPLEMENTED ──
    #
    # `system-to-os-outputs` attaches EVERY host of the system to the
    # flake-system scope. Left in place beside policy 2, a householded host
    # gets two scopes -- `host=secondpc,system=…` and
    # `household=ivymectin,host=secondpc,system=…`, distinct because
    # `mkScopeId` is the sorted key=value render of the whole ctx -- and
    # therefore two instantiate specs on one output path. den's
    # `disambiguate` (nix/lib/aspects/fx/edges/instantiate.nix) handles that
    # by `lib.warnIf … "keeping last"`: one of the two resolutions wins by
    # attribute order and the other's content is discarded. Not a merge, and
    # not a dedup.
    #
    # So the choice is between excluding den's policy or letting an attrset
    # ordering decide which walk renders each house's machines. Schema-tier
    # excludes register at the resolved kind's scope BEFORE that scope's
    # includes are walked (den nix/lib/aspects/fx/aspect/children.nix
    # `registerConstraints`, sequenced ahead of `emitIncludes`), and
    # `dispatch-policies` filters by them, so this suppresses the policy at
    # the only scope it ever fires at.
    #
    # `system-to-hm-outputs` is a separate policy and is untouched: standalone
    # homes keep resolving under flake-system.
    den.schema.flake-system.excludes = [ den.policies.system-to-os-outputs ];

    den.schema.household.includes = [ den.policies.household-to-hosts ];
  };
}
