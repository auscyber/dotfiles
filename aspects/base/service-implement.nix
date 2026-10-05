{
  den,
  lib,
  config,
  ...
}:
# WHAT A SERVICE DOES, FOLDED ONTO THE HOST THAT RUNS IT.
#
# `den.serviceTypes.<class>.implement` is module content written as a function of
# what the service RESOLVED:
#
#   implement = { tcp, cache, ... }: {
#     nix.settings.trusted-substituters =
#       [ "${tcp.protocol}${tcp.address}:${toString tcp.port}/${cache.serviceName}" ];
#   };
#
# `tcp` is the LINK -- the carrier's address, the scheme its encryption implies,
# and the edge's own `shared` attributes, which is where the port lives because
# "the link using tcp has a port" and a port is neither end's. `cache` is the
# ancestor, bound by the label its DEFINER gave it. So the body names no carrier
# and the same line is right over loopback and over the lan.
#
# ── WHY THIS FILE EXISTS AT ALL, which is the part worth reading ──
#
# `den.schema.host` has two spellings and they are not equivalent:
#
#   den.schema.host = { ... }: { options.x = ...; includes = [ ... ]; };   FUNCTION
#   den.schema.host.includes = [ ... ];                                   ATTR
#
# `includes` written inside the FUNCTION form is SILENTLY IGNORED. Traced, not
# guessed: a `builtins.trace` in an aspect added that way never printed while
# forcing `systemd.services`, and a `throw` placed in
# `den.policies.service-aspects`' own returned list never fired either. Both
# `options.runs` and `options.serves` survive from two different function-form
# definitions, so the OPTIONS merge fine -- it is only `includes` that goes
# nowhere.
#
# The consequence is bigger than this file: `den.policies.service-aspects` --
# the policy §5d probed and the whole "a service aspect resolves in its own
# scope" story rests on -- HAS NEVER RUN HERE. It is attached inside the
# function form in ./service.nix. Nothing it was supposed to deliver was ever
# delivered, and because every service type sets `aspect = null`, its first arm
# produced nothing anyway, so there was no symptom to notice.
#
# den also refuses both forms for one key ("attribute 'den.schema.host' already
# defined"), so the attr form needs a file of its own. This is it.
#
# ── ONE ASPECT THAT ITERATES, RATHER THAN A SCOPE PER SERVICE ──
#
# `{ host, ... }` binds, so the aspect reads the registry and folds every
# implementing service placed on this host. That is §3a's "service-keyed class"
# answer, and it needs no `{ service, ... }` binding at all -- the same move
# ./celler/server.nix already makes by reading `children` as DATA instead of
# asking for a scope per cache.
#
# It also makes §3a's collision impossible rather than avoided: one aspect
# resolved once per host cannot declare a forward twice, which is exactly what
# resolving a per-service aspect in a second scope did
# (`den.fwd."provision/nixos/services/kanidm/provision" is already declared`).
{
  # ── PASS THE SERVICE'S CLASS CONTENT STRAIGHT TO DEN ──
  #
  # A serviceType IS an aspect with relations. Its `nixos` / `homeManager` /
  # `darwin` keys are already exactly what den routes, so there is nothing to
  # route here: resolve the service's own content as an aspect, with its
  # `bindings` as ctx, and den delivers each class where that class goes.
  #
  # THIS REPLACED TWO HAND-WRITTEN ARMS -- a `nixos` fold and a
  # `provides.to-users.homeManager` fold beside it. That was a second
  # implementation of den's class routing, and it would have needed a third for
  # `darwin` and a fourth for whatever comes next. den already knows a host from
  # a user from a Mac.
  #
  # CTX IS HOW ARGUMENTS ARRIVE, which §5d proved: ctx entries bind as module
  # args, which is how `{ service, ... }` works. Passing `bindings` the same way
  # makes a body read `{ postgres, http, cache, ... }` with no wrapper and no
  # partial application -- den's mechanism rather than gen-bind's.
  #
  # ONLY THE CLASS KEYS are handed over: an instance also carries `uses`,
  # `gives`, `host` and the derived fields, which are not classes.
  den.schema.host.includes = [ den.policies.service-content ];

  den.policies.service-content =
    {
      host ? { },
      ...
    }:
    let
      classes = [
        "nixos"
        "darwin"
        "homeManager"
      ];

      mine = lib.filterAttrs (_: s: s.host != null && s.host == (host.name or null)) config.den.services;

      contentOf = s: lib.filterAttrs (_: v: v != null) (lib.genAttrs classes (c: s.${c} or null));
    in
    lib.mapAttrsToList (
      _: service:
      den.lib.policy.resolve.to.withIncludes "service" [ (contentOf service) ] (
        { inherit service; } // service.bindings
      )
    ) (lib.filterAttrs (_: s: contentOf s != { }) mine);
}
