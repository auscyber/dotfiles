{
  den,
  lib,
  config,
  capability,
  checkLib,
  ...
}:
# ── WHAT A HOST NEEDS FROM THE REST OF THE HOUSE, AS CHECKS IT CAN RUN ──
#
# A household service is declared once and served somewhere (./household.nix,
# ./service.nix). When a host CONSUMES one it does not serve -- auspc's
# turntable streaming to Music Assistant on secondpc -- the machine that
# answers is on the other end of a wire, and the thing that goes wrong is not a
# misconfiguration: the peer is off, and this box should come up without the
# aspect that needed it.
#
# That is exactly what ../framework/requires.nix is for, and what
# ../tooling/nh-checked.nix already does with the answer. What was missing is
# that nobody wrote the checks: `celler-host` has one, hand-written, and
# everything else that crosses a machine boundary had none.
#
# So they are DERIVED, from the declaration the consumer already made:
#
#   takes.udp = { name = "music-assistant"; mtu = m: m >= 1500; };
#
# ./capability.nix's `dial` turns that into an address and a port -- which
# giver (applying `reach`), then which carrier between the two boxes. If the
# answer is on another host, this emits a check that it answers there.
#
# ── WHY IT IS KEYED BY THE TAKING ASPECT ──
#
# ../framework/requires.nix collects `requires` per aspect and
# ../tooling/nh-checked.nix matches the FAILED ASPECT NAMES against what each
# specialisation excludes. The aspect that breaks when the peer is gone is the
# one that TAKES -- `vban-turntable`, not `music-assistant`, which is not even
# on this machine -- so that is the key, and `den.aspects.auspc`'s
# `no-turntable` profile is what excludes it. The check belongs to the thing
# that needs it, which is ../framework/requires.nix's own rule.
#
# ── AND WHY ONLY HOUSEHOLD SERVICES ──
#
# Not every cross-host take wants a check in front of every build. `celler`
# already has one, written where the flow's constraints are (../base/celler/
# push.nix), and a second, blunter copy from here would fail for reasons that
# file deliberately excludes -- and would be keyed to `celler-default`, which
# no specialisation excludes, so a dead celler would REFUSE the build instead
# of selecting `no-celler`.
#
# The household is the boundary that makes this safe and is the one the user
# draws: a service the house declares, placed on a machine in the house, with
# `reach = "household"` on the aspect that implements it. A take that resolves
# to something else resolves exactly as before and emits nothing.
let
  inherit (lib) mkOption types;

  hostsOf = lib.concatMapAttrs (_: hs: hs) den.hosts;

  # ── WHERE EACH HOUSEHOLD SERVICE ACTUALLY IS ──
  #
  # `h.services.<n>.enable`, the CLAIM, rather than `h.serves`: the claim is
  # plain declared data, while `serves` carries the throw for a claim the
  # household does not declare. That throw is right where it is and wrong here
  # -- a half-written host should not take the reachability checks of every
  # other machine down with it -- and forcing it from a walk over the whole
  # fleet is the kind of thing that turns a flake-level query into a host eval.
  placements = lib.concatLists (
    lib.mapAttrsToList (
      _: h:
      let
        household = h.household or null;
        declared = if household == null then { } else household.services;
        claimed = lib.attrNames (lib.filterAttrs (_: s: s.enable) (h.services or { }));
      in
      lib.concatMap (
        n:
        let
          svc = declared.${n} or null;
        in
        lib.optional (svc != null && svc.aspect != null) {
          service = n;
          aspect = svc.aspect;
          host = h;
          household = household.name;
        }
      ) claimed
    ) hostsOf
  );

  # The placement a resolved dial landed on, or null. Matched on BOTH halves:
  # the aspect that furnished the link and the host it was found on, because
  # one aspect can implement a service on several machines and "is this a
  # household service" is a question about the pair.
  placementOf =
    d:
    lib.findFirst (
      p: p.aspect.name == (d.via.name or null) && p.host.name == (d.host.name or null)
    ) null placements;

  # ── A FAILED RESOLUTION IS NOT A FAILED CHECK ──
  #
  # `dial` reaches `judge`, which THROWS on a take whose clause names something
  # the capability neither furnishes nor accepts. That throw is correct at the
  # point of use -- it is a typo in a declaration -- but this walks every take
  # on every host, so raising it from here would turn one bad clause anywhere
  # into a flake that cannot answer `buildConditions` for anybody. The aspect's
  # own content still throws it, where it reads.
  try =
    v:
    let
      r = builtins.tryEval v;
    in
    if r.success then r.value else null;

  checkFor =
    host: aspect: cap: take:
    let
      d = try (capability.dial cap take host);
      p = if d == null then null else placementOf d;
    in
    if d == null || d.sameHost || p == null || d.port == null then
      null
    else
      let
        url = "${if d.protocol == null then "http://" else d.protocol}${d.address}:${toString d.port}";

        # ── WHAT "IT ANSWERS" MEANS, PER PROTOCOL ──
        #
        # NO `curl -f`: a gated service replying 401 or redirecting to the
        # identity provider IS the service answering, and `-f` turns both into
        # a failure -- which would select a specialisation because a login
        # page appeared. The question here is whether anything is listening,
        # not whether this host is allowed in.
        #
        # UDP CANNOT BE ASKED, only told: there is no handshake, so `nc -zu`
        # reports success for a silent port. It still catches the case this
        # exists for -- a receiver that is off the LAN entirely answers no ARP
        # and the send fails outright -- and that is stated rather than
        # implied, because a passing udp check means less than a passing tcp
        # one.
        script =
          if cap == "udp" then
            "nc -z -u -w 5 ${lib.escapeShellArg d.address} ${toString d.port}"
          else if d.protocol != null && lib.hasPrefix "http" d.protocol then
            "curl -sS --max-time 5 -o /dev/null ${lib.escapeShellArg url}"
          else
            "nc -z -w 5 ${lib.escapeShellArg d.address} ${toString d.port}";
      in
      lib.nameValuePair "${p.service}-reachable" {
        description = "`${p.service}` answers on ${p.host.name} at ${d.address}:${toString d.port}, over ${d.link.carrier or "?"}";
        inherit script;
      };

  # Every check one host's aspects imply, keyed by aspect then by check.
  derivedFor =
    host:
    lib.filterAttrs (_: v: v != { }) (
      lib.mapAttrs (
        _: aspect:
        lib.listToAttrs (
          lib.filter (x: x != null) (lib.mapAttrsToList (checkFor host aspect) (aspect.takes or { }))
        )
      ) (lib.filterAttrs (_: a: (a.takes or { }) != { } && host.hasAspect a) den.aspects)
    );

  # ── A TAKE THAT RESOLVES FROM NOWHERE, FOR THE WHOLE FLEET AT ONCE ──
  #
  # `capability.linkTo` already throws with a derived message when nothing
  # reachable gives a capability -- but it throws for ONE host, while that host
  # is being built, which is the last moment it still helps and only ever
  # reports the first broken route. This asks the same question of every take in
  # the tree before anything is built.
  #
  # ── THE UNIT IS THE TAKE, NOT THE (HOST, TAKE) PAIR ──
  #
  # Measured, and it is the whole reason this check is worth having rather than
  # noise: a take failing to resolve on SOME host is normal and deliberate.
  # `photos` is a person aspect, so ../base/person.nix delivers it to
  # every account that person has -- four machines that serve no files -- and
  # ../services/shares.nix's header states the intent outright ("jonesy is
  # deliberately included even though she has no account on secondpc yet: the
  # grant is then already correct the moment she gets one, and until then it
  # contributes nothing rather than being wrong"). Twenty such pairs do not
  # resolve today and every one of them is correct.
  #
  # What is never correct is a take that resolves on NO host that has the
  # aspect. That is a route into nothing: either the capability is given
  # nowhere, or the identity clauses name an instance that does not exist, or
  # every giver is outside the take's `reach`.
  #
  # ── AND WHY `resolveFrom` RATHER THAN `dial` ──
  #
  # `dial` returns null whenever the resolved link has no ADDRESS, which is the
  # normal case for a capability that is not routed -- postgres over a unix
  # socket, `gated` as a relation between a vhost and an identity provider. 76
  # of 85 takes in this tree come back null from `dial` and 76 of them are
  # fine. `resolveFrom` is the half that answers "which giver, on which host",
  # which is the half `linkTo` throws about.
  takeRoutes =
    let
      sites = lib.concatLists (
        lib.mapAttrsToList (
          aspectName: aspect:
          lib.mapAttrsToList (cap: take: {
            inherit
              aspectName
              aspect
              cap
              take
              ;
          }) (aspect.takes or { })
        ) (lib.filterAttrs (_: a: (a.takes or { }) != { }) den.aspects)
      );

      # ── ONLY ROUTED CAPABILITIES HAVE A ROUTE TO CHECK ──
      #
      # A contribution capability has no giver by construction -- `share`,
      # `samba-user`, `home-assistant`: the take is a marker and a collector
      # reads it with `takersOn`/`contributions`. Asking whether it resolves is
      # asking the wrong question, and this check answered it with a finding
      # that was true and not a fault. `routed` is declared on the capability
      # (./capability-types.nix) so the answer lives with the capability rather
      # than as an exception list here.
      # DERIVED, not a declared flag. `isRouted cap` is `elem "address"
      # (capClosure cap)`; the `routed` option this used to read was a second
      # copy that disagreed with it on 21 of 30 capabilities, and this check
      # took the wrong side -- so `share`, `gated`, `postgres` and six others
      # were asked whether they resolve to a giver when they furnish no address
      # and never will.
      routedSites = lib.filter (s: capability.isRouted s.cap) sites;

      # WHO, and nothing about the wire -- the same split `dial` makes, and for
      # the same reason: the flow clauses (`mtu`, `maxBodyBytes`) are judged
      # against a CARRIER and reject every candidate when applied here, which
      # would make this check fire on all 85 takes.
      whoTakeOf =
        take:
        let
          t = capability.emptyTake // take;
        in
        capability.emptyTake
        // {
          inherit (t)
            name
            within
            sameHost
            where
            ;
        };

      findingFor =
        site:
        let
          on = lib.filter (h: h.hasAspect site.aspect) checkLib.hostEntities;
          whoTake = whoTakeOf site.take;

          # PER HOST, not `lib.any`. This was "one host resolving is the
          # answer", which passes a take that resolves nowhere on ten hosts so
          # long as it resolves on the eleventh -- and that is exactly what hid
          # twelve dead `share` sites behind one success on secondpc. A take
          # sits on a host because that host needs it, so each host that has it
          # is its own question.
          failedOn = lib.filter (h: (capability.resolveFrom h site.cap whoTake) == [ ]) on;
          resolves = failedOn == [ ];

          # WHERE IT IS GIVEN, named rather than asserted. `providersFor` is the
          # entity-side lookup, so this is the honest "it exists, just not in
          # reach" half of the message -- and empty means the stronger thing.
          placed = lib.unique (
            lib.concatMap (
              h: lib.optional ((capability.providersFor h site.cap) != { }) (h.name or "?")
            ) checkLib.hostEntities
          );

          named = (capability.emptyTake // site.take).name;
        in
        if on == [ ] || resolves then
          [ ]
        else
          [
            (
              "`${site.aspectName}` takes `${site.cap}`"
              + lib.optionalString (named != null) " named `${named}`"
              + " and nothing gives it within reach of any host that has the aspect (${
                 lib.concatMapStringsSep ", " (h: h.name or "?") on
               }). "
              + (
                if placed == [ ] then
                  "No host in the fleet gives `${site.cap}` at all -- place a giver, or drop the take."
                else
                  "`${site.cap}` is given on ${lib.concatStringsSep ", " placed}, so either that is outside this capability's `reach` from those hosts"
                  + lib.optionalString (named != null) ", or no giver there furnishes the instance `${named}`"
                  + "."
              )
            )
          ];
    in
    lib.concatMap (
      site:
      checkLib.probe "${site.aspectName}/${site.cap}"
        "capability.resolveFrom <host> \"${site.cap}\" <take>"
        (findingFor site)
    ) routedSites;
in
{
  config.den.checks.take-routes = {
    description = "Every take resolves to a giver on at least one host that declares it.";
    findings = takeRoutes;
    fix = "Place a giver of that capability on a host within its `reach`, correct the take's instance name, or drop the take. ./capability-types.nix declares each capability's `reach`; `capability.explainTake` prints the per-carrier table for one take.";
  };

  # ../framework/requires.nix declares `den.derivedRequires` and folds it into
  # the same view the on-box `requires-check` command and `flake.buildConditions`
  # both read, so a derived check and a hand-written one are indistinguishable
  # to everything downstream -- which is the point: ../tooling/nh-checked.nix
  # should not have two ways to learn that a peer is gone.
  config.den.derivedRequires = lib.filterAttrs (_: v: v != { }) (
    lib.mapAttrs (_: derivedFor) hostsOf
  );

  # ── THE SAME ANSWER, WITHOUT A HOST EVAL ──
  #
  # `nix eval --impure --json .#debug.config --apply 'cfg:
  #    cfg.den.householdReach.auspc'`
  #
  # Which household services each host reaches for, where they are and what is
  # carrying it -- the question "why is this box streaming to nothing" asks,
  # answered off entities and givers in seconds. ../../CLAUDE.md's capability
  # loop is the same move; this is it already composed.
  options.den.householdReach = mkOption {
    type = types.raw;
    readOnly = true;
    default = lib.mapAttrs (
      _: host:
      lib.concatMapAttrs (
        aspectName: checks:
        lib.mapAttrs' (check: c: lib.nameValuePair check (c.description + " [${aspectName}]")) checks
      ) (config.den.derivedRequires.${host.name} or { })
    ) hostsOf;
    description = "Per host, the household services its aspects reach across machines.";
  };
}
