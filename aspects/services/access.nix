{
  den,
  lib,
  config,
  checkLib,
  ...
}:
# Who may reach which service, at what level -- declared once and propagated to
# every place that has to enforce it.
#
# Two facts, in two places, and neither side has to know the other:
#
#   the SERVICE declares what access it exposes   household.services.<s>.access
#   a GROUP of services declares the same          household.serviceGroups.<g>.access
#   the MEMBERSHIP declares who gets what level   members.<p>.services.<target>.access
#
# A grant names either a service or a GROUP. A group is just a tag on services
# (../../lib/service-type.nix `group`), so one grant on `arr` emits a single
# `app-arr-<level>` and adds it to the gate of every tagged service this host
# serves -- six services, one grant, one kanidm group.
#
# Both now live on the HOUSEHOLD (../base/household.nix, ../../lib/service-type.nix),
# which is what lets this collector stop guessing: it walks `host.serves` -- the
# services this machine both claims and that exist -- and reads each one's own
# derived `users`. So it only ever emits groups for services that are actually
# running here, and it never writes a filter over the household itself.
#
# The grant is on the MEMBERSHIP, not the person: access is household-scoped by
# nature (admin of your own house, guest in another), while what travels with the
# human is their aspects. ../base/household.nix carries the same note.
#
# It emits ONE THING: `provision.groups.app-<target>-<level>`, holding the
# people granted it. Nothing else.
#
# ── WHAT A GRANT MEANS IS THE APP'S OWN BUSINESS ──
#
# This file used to also write `gateway.services.<service>.extraGroups` for
# every grant -- one hard-coded mechanism, the nginx gate's allowed-groups,
# applied centrally on behalf of every service. That is why eight of ten grants
# did nothing: a grant on a real OIDC client (`oauthProxy = false`) or on a
# service with its own accounts has no gate to open, and the only realisation
# on offer was a gate.
#
# So each app realises its own grant, in its own file, through the forward it
# already uses:
#
#   a gated app      `gated.<n>.extraGroups = accessLib.groupsFor "<n>"`
#   an OIDC client   `kanidmClients.<n>.scopes = accessLib.groupsFor "<n>"`
#   its own accounts `gives.service.externallyManaged = true`
#
# `accessLib` below is what they read the group name from, and
# `den.checks.access-grants-land` is what refuses to let a grant realise
# nothing again.
#
# WAS A QUIRK, NOW A TRAVERSAL. Each service aspect used to write
# `access = _: { service; levels; defaultLevel; }` and this file collected the
# emissions. The quirk is gone: `access` is a field on the service entity, so the
# levels sit next to the port and the subdomain instead of in a parallel channel,
# and a service that no host serves contributes nothing rather than emitting a
# group for a service that is not running.
#
# STILL ADDITIVE AT THE GATE: `gateway.humanGroups` is `[ "media-users" ]`
# (../hosts/secondpc/default.nix) and ../persons/ivy.nix is in it, so a gated
# service stays reachable by that group whatever the grants say. Narrowing
# `humanGroups` to `[ ]` is what turns the gate into a real one, and it is a
# separate deliberate change. The OIDC half is NOT additive: a client's
# `scopes` REPLACES `media-users`, so a grant is the only way in there.
let
  groupFor = service: level: "app-${service}-${level}";

  # ── WHAT A GRANT'S LEVEL CAN BE WRONG ABOUT, IN ONE PLACE ──
  #
  # An unknown level is the whole failure this indirection exists to prevent:
  # the member looks granted, the group is never created, and the service stays
  # shut with no error anywhere. The rule was written inside the `nixos` body
  # below, where it can only see `host.serves` -- the services the EVALUATING
  # machine runs. A grant on a service another box serves, or on one nothing
  # serves yet, was never level-validated at all, and coverage was total only
  # because secondpc happens to be the one host including this aspect.
  #
  # So the rule lives here and both readings share it: the host-side one throws
  # (a misconfigured gate must not be built), and the fleet-wide check below
  # reports every household's grants at once without evaluating a host.
  #
  # Returns null when the level is fine, so the caller decides what to do with
  # a problem. `levels`/`defaultLevel` through `or` because `access` is a raw
  # value a service writes, not a typed submodule -- the keys are genuinely
  # absent when unset.
  levelsOf = acc: acc.levels or [ "member" ];
  defaultOf = acc: acc.defaultLevel or null;

  levelProblem =
    {
      what,
      access,
      request,
    }:
    let
      lvl = if request.access == "" then defaultOf access else request.access;
    in
    if lvl == null then
      "'${what}' declares no default level, so one must be given. Levels: ${lib.concatStringsSep ", " (levelsOf access)}."
    else if !lib.elem lvl (levelsOf access) then
      "'${lvl}' is not a level '${what}' exposes. Levels: ${lib.concatStringsSep ", " (levelsOf access)}."
    else
      null;

  # Every (household, member, target) grant in the tree, with the declaration
  # it is a grant ON. Entity data only, so this is the fleet-wide view that
  # costs no host evaluation.
  allGrants = lib.concatLists (
    lib.mapAttrsToList (
      hname: h:
      lib.concatLists (
        lib.mapAttrsToList (
          person: m:
          lib.mapAttrsToList (target: request: {
            household = hname;
            inherit person target request;
            where = "den.households.${hname}.members.${person}.services.${target}";
            service = h.services.${target} or null;
            group = h.serviceGroups.${target} or null;
          }) m.services
        ) h.members
      )
    ) config.den.households
  );

  # What a grant TARGETS declares, whichever of the two it is.
  accessOf =
    g:
    if g.service != null then
      g.service.access
    else if g.group == null then
      null
    else
      g.group.access;

  # Every grant with its level resolved, or null where the level is wrong --
  # `den.checks.access-levels` below is the voice for that, so this stays
  # quiet and simply contributes no group.
  resolvedGrants = map (
    g:
    let
      acc = accessOf g;
      lvl =
        if acc == null then
          null
        else if
          levelProblem {
            what = g.target;
            access = acc;
            inherit (g) request;
          } != null
        then
          null
        else if g.request.access == "" then
          defaultOf acc
        else
          g.request.access;
    in
    g // { level = lvl; }
  ) allGrants;
in
{
  den.aspects.access = {
    includes = [ den.aspects.gateway ];

    nixos =
      {
        lib,
        host,
        ...
      }:
      let
        # Every (person, target, level) grant, validated, where a TARGET is
        # either a service or a service GROUP.
        #
        # An unknown level is a THROW. A free string that quietly matches
        # nothing is the whole failure this indirection exists to prevent: the
        # member looks granted, the group is never created, and the service
        # stays shut with no error anywhere.
        # ONE RULE, TWO READINGS. `levelProblem` is at file level so the
        # fleet-wide `den.checks.access-levels` below judges a grant exactly as
        # this does -- a second copy here is how the two would come to disagree.
        checkLevel =
          {
            where,
            what,
            access,
            request,
          }:
          let
            problem = levelProblem { inherit what access request; };
          in
          if problem != null then
            throw "${where}: ${problem}"
          else if request.access == "" then
            defaultOf access
          else
            request.access;

        household = host.household;
        householdName = if household == null then "<none>" else household.name;
        members = if household == null then { } else household.members;
        declaredServices = if household == null then { } else household.services;
        declaredGroups = lib.filterAttrs (_: g: g.access != null) (
          if household == null then { } else household.serviceGroups
        );

        # Per-SERVICE grants: from each served service's own derived `users`,
        # so no aspect writes a filter over the household.
        serviceGrants = lib.concatLists (
          lib.mapAttrsToList (
            service: svc:
            lib.mapAttrsToList (person: u: {
              key = service;
              # ── THE KANIDM NAME, NOT THE PERSON'S KEY ──
              # These land in `provision.groups.<g>.members`, which kanidm
              # matches against ACCOUNT names. The two are the same string for
              # most people and deliberately are not for Ivy, whose entity is
              # `ivy` and whose account is `auscyber` (../base/person.nix).
              # Using the key here would grant a group to a person kanidm has
              # never heard of -- silently, since a member list is just
              # strings.
              person = u.person.kanidm.name;
              level = checkLevel {
                where = "den.households.${householdName}.members.${person}.services.${service}";
                what = service;
                access = svc.access;
                request = u.request;
              };
            }) svc.users
          ) (lib.filterAttrs (_: s: s.access != null) host.serves)
        );

        # Per-GROUP grants: one grant opens every service tagged with that
        # group that THIS host serves. Read from the members directly rather
        # than from `service.users`, because a group is a tag and no single
        # service's user set contains it.
        groupGrants = lib.concatLists (
          lib.mapAttrsToList (
            group: g:
            lib.concatLists (
              lib.mapAttrsToList (
                person: m:
                lib.optional (m.services ? ${group}) {
                  key = group;
                  # The ACCOUNT name, as above -- `person` here is the member
                  # key, which is the human and not the login.
                  person = m.person.kanidm.name;
                  level = checkLevel {
                    where = "den.households.${householdName}.members.${person}.services.${group}";
                    what = group;
                    access = g.access;
                    request = m.services.${group};
                  };
                }
              ) members
            )
          ) declaredGroups
        );

        grants = serviceGrants ++ groupGrants;

        # A service may carry a `group` tag the household never declares. Left
        # unchecked that is silent: the tag matches no group, the service joins
        # no grant, and it looks granted from the household's side while being
        # reachable only by `humanGroups`. Same failure mode the level check
        # exists to prevent, one level up.
        #
        # Checked against this host's SERVED services, since that is the set
        # whose gates are emitted here.
        untaggedGroups = lib.unique (
          lib.concatLists (
            lib.mapAttrsToList (
              svc: s:
              lib.optional (s.group != null && !(household.serviceGroups ? ${s.group})) "${svc} -> ${s.group}"
            ) host.serves
          )
        );

        # A member may name something the household declares as neither a
        # service nor a service group. Checked against the household's own
        # declarations rather than against what this host serves: a grant for a
        # service another machine serves is legitimate and must not throw.
        validTargets = lib.attrNames declaredServices ++ lib.attrNames declaredGroups;

        unknownRequests = lib.concatLists (
          lib.mapAttrsToList (
            person: m: map (s: "${person} -> ${s}") (lib.subtractLists validTargets (lib.attrNames m.services))
          ) members
        );

        checked =
          if untaggedGroups != [ ] then
            throw "den.households.${householdName}: these services carry a `group` the household does not declare in `serviceGroups`: ${lib.concatStringsSep ", " untaggedGroups}. It declares: ${
              let
                gs = lib.attrNames household.serviceGroups;
              in
              if gs == [ ] then "no groups at all" else lib.concatStringsSep ", " gs
            }."
          else if unknownRequests == [ ] then
            grants
          else
            throw "den.households.${householdName}: these members request services the household declares as neither a service nor a service group: ${lib.concatStringsSep ", " unknownRequests}. It declares services: ${lib.concatStringsSep ", " (lib.attrNames declaredServices)}; groups: ${
              let
                gs = lib.attrNames declaredGroups;
              in
              if gs == [ ] then "none" else lib.concatStringsSep ", " gs
            }.";

        # kanidm group name -> the people in it. Keyed on the GRANT's target,
        # so a group grant makes one `app-arr-admin` rather than one group per
        # tagged service.
        groupMembers = lib.foldl' (
          acc: g:
          let
            k = groupFor g.key g.level;
          in
          acc // { ${k} = lib.unique ((acc.${k} or [ ]) ++ [ g.person ]); }
        ) { } checked;

      in
      {
        # Through `nixos`, NOT the `provision` class. den class content merges
        # one level deep, so a second aspect writing `provision.groups` replaces
        # the first aspect's whole `groups` tree -- which silently deletes
        # sso.nix's `media-users`/`idm_admins` and every `svc-*` group
        # ./gateway.nix generates. Confirmed empirically; ./nextcloud.nix
        # documents the same trap for `systems`.
        services.kanidm.provision.groups = lib.mapAttrs (_: members: {
          inherit members;
        }) groupMembers;

        # ── AND NO `gateway.services` ──
        #
        # The gate's allowed-groups used to be written here for every grant,
        # which made the gate the only realisation this model had. It is
        # grocy's and paperless's and each *arr app's own mechanism now,
        # declared beside the take that creates it. See this file's header.
      };
  };

  # ── A SERVICE THE HOUSEHOLD DECLARES AND NOTHING IMPLEMENTS ──
  #
  # ../../lib/service-type.nix's `aspect` option says this file reports it, and
  # until now it did not -- a documented check that does not exist. The chain is
  # entirely silent: nothing gives `service` under that name, so `aspect` is
  # null, so `access` and `group` are null, so the `access != null` filter above
  # drops it from `serviceGrants`, `untaggedGroups` is empty because `group` is
  # null too, and `unknownRequests` does not fire because the household DOES
  # declare the name. Every member granted it looks granted and no kanidm group
  # is ever created.
  #
  # Flake-level because it is a question about DECLARATIONS -- the household's
  # services and whichever aspect gives one -- and because a service nothing
  # serves yet is exactly the case a host-side check cannot see.
  den.checks.household-service-providers = {
    description = "Every service a household declares resolves an implementing aspect.";
    findings = lib.concatLists (
      lib.mapAttrsToList (
        hname: h:
        lib.concatLists (
          lib.mapAttrsToList (
            sname: svc:
            let
              granted = lib.attrNames (lib.filterAttrs (_: m: m.services ? ${sname}) h.members);
            in
            lib.optional (svc.aspect == null) (
              "den.households.${hname}.services.${sname} resolves no implementing aspect: no aspect gives `service` under that name and there is no `den.aspects.${sname}` either. Its `access` and `group` are therefore null, ./access.nix emits no group for it"
              + (
                if granted == [ ] then
                  ", and nothing is granted on it yet."
                else
                  ", and the ${toString (lib.length granted)} grant(s) on it (${lib.concatStringsSep ", " granted}) are silently dropped -- those members look granted and no group is ever created."
              )
            )
          ) h.services
        )
      ) config.den.households
    );
    fix = "Give the implementing aspect `gives.service.<name>` (which is how `home-assistant` finds `den.aspects.homeassistant` with nothing written down), name it with `services.<n>.aspect`, or drop the service and the grants on it.";
  };

  # ── AND EVERY GRANT'S LEVEL, INCLUDING THE ONES NO HOST SEES ──
  #
  # `checkLevel` above throws for a bad level, but only for the services the
  # EVALUATING host serves -- so a grant on a service another machine serves, or
  # on one nothing serves yet, was never validated. Same rule (`levelProblem`),
  # asked of every household's declarations instead, which is the treatment
  # `unknownRequests` already gets and for the same reason.
  #
  # A grant whose target the household declares as neither a service nor a group
  # is NOT reported here: that is `unknownRequests`, which is already checked
  # against household declarations and would be two voices on one fact.
  den.checks.access-levels = {
    description = "Every member's grant names a level the service or service group it targets exposes.";
    findings = lib.concatMap (
      g:
      let
        access = if g.service != null then g.service.access else (g.group.access or null);
        problem =
          if access == null then
            null
          else
            levelProblem {
              what = g.target;
              inherit access;
              inherit (g) request;
            };
      in
      # `access == null` IS NOT A LEVEL PROBLEM. It means the target exposes no
      # access at all, which for a service means nothing implements it -- the
      # finding `household-service-providers` above already makes, with the
      # chain spelt out. Reporting it twice from two checks is worse than once
      # from the one that can explain it.
      if problem == null then [ ] else [ "${g.where}: ${problem}" ]
    ) allGrants;
    fix = "Use one of the levels the target declares in its `access.levels`, or give the target a `defaultLevel` so an empty grant is meaningful.";
  };

  # ── THE GROUP NAMES, FOR THE APPS THAT NOW OPEN THEIR OWN DOORS ──
  #
  # A file-level handle rather than a per-app literal: `app-<target>-<level>`
  # is this file's spelling, it is the name the `provision.groups` above are
  # created under, and an app writing it out would be a string two files have
  # to keep in agreement.
  #
  # ONLY THE GROUPS A GRANT ACTUALLY PRODUCES, not one per declared level. A
  # kanidm scope map naming a group nobody provisioned fails the provisioning
  # run outright, and `groupMembers` above only creates a group that has
  # somebody in it -- `app-vikunja-user` is declared by vikunja and granted to
  # nobody, so it does not exist and must not be scoped.
  #
  # Keyed on the GRANT'S TARGET, so `groupsFor "arr"` answers for the tag that
  # six services carry and `groupsFor "grocy"` for the one service. An app
  # asks about whichever of the two its grant comes through.
  _module.args.accessLib = {
    inherit groupFor;

    groupsFor =
      target:
      lib.unique (
        map (g: groupFor target g.level) (
          lib.filter (g: g.target == target && g.level != null) resolvedGrants
        )
      );
  };

  # ── AND THAT EVERY GRANT LANDS SOMEWHERE ──
  #
  # The failure this whole file exists to prevent, one level up from the level
  # check: the group is created, the member is in it, and NOTHING READS IT.
  # Eight of the ten grants in this tree were in that state -- four real OIDC
  # clients whose `oauthProxy = false` means the gate checks no group, and
  # plex, mail and nextcloud with no gate at all -- and every one of them
  # looked granted from the household's side.
  #
  # Three ways to land, which is the whole vocabulary of realisation:
  #
  #   a GATE          an aspect's `gated.<n>.extraGroups` names the group
  #   a SCOPE MAP     an aspect's `kanidmClients.<n>.scopes` names it
  #   NOT FROM NIX    the service says `gives.service.externallyManaged`
  #
  # Read off ASPECT DECLARATIONS, so it costs no host evaluation and covers a
  # service no host serves yet -- the same reason `access-levels` above is
  # flake-level.
  den.checks.access-grants-land = {
    description = "Every member's grant is realised by a gate, a kanidm scope map, or an explicit externally-managed declaration.";
    findings =
      let
        # Every group name any aspect declares it honours, and where it says
        # so. `isAttrs` because class content may be a function, and a
        # function here is not a declaration this can read.
        declaredIn =
          field: inner:
          lib.concatLists (
            lib.mapAttrsToList (
              aname: a:
              let
                content = a.${field} or { };
              in
              lib.optionals (lib.isAttrs content) (
                lib.concatLists (
                  lib.mapAttrsToList (
                    ename: e:
                    map (grp: {
                      group = grp;
                      where = "den.aspects.${aname}.${field}.${ename}.${inner}";
                    }) (if lib.isAttrs e then e.${inner} or [ ] else [ ])
                  ) content
                )
              )
            ) config.den.aspects
          );

        realisers = declaredIn "gated" "extraGroups" ++ declaredIn "kanidmClients" "scopes";

        # A service may say the grant is honoured outside nix entirely. For a
        # GROUP-tag grant there is no one service to ask, so the tag is only
        # ever realised by the aspects that carry it.
        externallyManaged = g: g.service != null && g.service.externallyManaged;
      in
      lib.concatMap (
        g:
        let
          want = groupFor g.target g.level;
          found = lib.filter (r: r.group == want) realisers;
        in
        if g.level == null || externallyManaged g || found != [ ] then
          [ ]
        else
          [
            "${g.where}: nothing realises '${want}'. The group is provisioned and ${g.person} is in it, and no aspect reads it -- so the grant opens nothing and looks correct from the household's side."
          ]
      ) resolvedGrants;
    fix = "On the aspect that implements the target, declare how a grant on it is honoured: `gated.<n>.extraGroups = accessLib.groupsFor \"<target>\"` for a service the nginx gate fronts, `kanidmClients.<n>.scopes = accessLib.groupsFor \"<target>\"` for a real OIDC client (which also NARROWS it -- the scope map replaces `media-users`), or `gives.service.externallyManaged = true` for a service whose accounts are not ours to grant.";
  };
}
