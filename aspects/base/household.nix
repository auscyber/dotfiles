{
  den,
  lib,
  config,
  serviceLibs,
  ...
}@args:
# A HOUSEHOLD is a place with people in it, and a MEMBER is one person's
# membership of one household -- not the person themselves.
#
# That split is the whole design, and it is forced by two requirements that
# pull in opposite directions:
#
#   * "drop a person from a household and their spaces, devices and entities go
#     with them" wants the household-scoped data NESTED, so deleting the
#     membership deletes everything hanging off it. Nothing to garbage-collect
#     and nothing to forget.
#
#   * "some people live in multiple places, or are a guest" means a PERSON
#     cannot be nested under a household -- they would become two unrelated
#     records for one human, which is precisely the problem ./person.nix exists
#     to solve for accounts.
#
# So the person stays top-level (`den.persons.<name>`, one record per human) and
# the MEMBERSHIP is what nests: `den.households.<h>.members.<person>`. A member
# derives its identity from the person of the same name and adds only what is
# true *in that household* -- which rooms are theirs, what they may reach there.
# One human in two households is two memberships and one person.
#
# `members` is declared BY HAND here rather than appearing from
# `den.schema.member.parent = "household"`. Checked empirically: a custom
# top-level collection does NOT get a pluralised sub-collection from the parent
# relation -- a household instance came back with only
# `_identity`/`_identityKeys`/`displayName`/`id_hash`/`name`. den's own
# `den.hosts.<sys>.<h>.users` exists because `nix/lib/entities/host.nix`
# declares `users = attrsOf (userType config)` in its `extraModules`; this file
# does the same thing for the same reason.
let
  inherit (lib) mkOption types;
  inherit (den.lib.policy) include;

  persons = config.den.persons;

  roomType = types.submodule (
    { name, ... }:
    {
      options = {
        name = mkOption {
          type = types.str;
          readOnly = true;
          default = name;
        };
        displayName = mkOption {
          type = types.str;
          default = name;
          description = "Human-facing name, e.g. the Home Assistant area label.";
        };
        floor = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = "Home Assistant floor this room sits on, when it declares floors.";
        };
        shared = mkOption {
          type = types.bool;
          default = false;
          description = ''
            A common space (kitchen, hallway) rather than someone's own room.
            A shared room's devices are reachable by every member; a non-shared
            one is reachable only by the members who claim it, which is what
            keeps the things in Ivy's room out of everybody else's.
          '';
        };
      };
    }
  );

  # The membership. Keyed by PERSON NAME, and the person must already exist --
  # a member is a person's presence in a household, never a second definition
  # of the human. A name with no matching person is a throw rather than an
  # empty record, because the silent version grants a nonexistent human access
  # to a real house.
  memberType =
    household:
    den.lib.schema.mkInstanceType {
      strict = true;
      extraModules = [
        (
          { name, config, ... }:
          {
            options = {
              # ── A CLASS, BECAUSE `hasAspect` NEEDS ONE ──
              #
              # den's modules/context/has-aspect.nix resolves the aspect tree
              # PER CLASS, so an entity with neither `class` nor `classes`
              # throws "entity has no `class` or `classes`" the moment anything
              # queries it. Measured: exactly what a member did once it became
              # an entity kind.
              #
              # DECLARED HERE AND NOT AS `den.schema.member.classes`. The kind
              # entry takes that key -- `den.schema.user.classes` is set in
              # ../framework/den.nix -- but the INSTANCE type is strict, so a
              # value with no matching option is refused, and den says so in as
              # many words: `STRICT MODE: "classes" is not declared on member`.
              # It works for users because den's own `userType`
              # (nix/lib/entities/host.nix) declares the option in its own
              # `extraModules`; this does the same thing in the same place.
              #
              # `member` is a class NOTHING RENDERS. A membership produces no
              # config of its own -- it exists so the household's people can be
              # asked about, which is a structural question -- and naming a real
              # class here (`homeManager`, say) would claim this scope renders a
              # home.
              classes = mkOption {
                type = types.listOf types.str;
                default = [ "member" ];
                description = "Classes this membership's aspect tree resolves for. Nothing renders them.";
              };

              household = mkOption {
                type = types.raw;
                readOnly = true;
                default = household;
                description = "The household this membership belongs to.";
              };

              person = mkOption {
                type = types.raw;
                readOnly = true;
                default =
                  persons.${name}
                    or (throw "den.households.${household.name}.members.${name}: no person '${name}' is declared. Members are keyed by person name and derive from ../persons/<name>.nix; add the person first, or correct the spelling.");
                description = "The person this membership derives from.";
              };

              # ── THE SUBSET IS THE TYPE ──
              #
              # This said "naming one that does not exist is an evaluation
              # error" while the type was `listOf str`, so it was not: an
              # unknown room name was accepted, `reachableRooms` carried it,
              # and whatever consumed that reached nothing. ../../CLAUDE.md's
              # rule is that a constraint belongs in the option's `type`, and
              # the set it has to be a subset of is right here -- `memberType`
              # takes the household's own config for exactly this kind of
              # question (see the note on `serviceType` below).
              #
              # NO RECURSION, and it was worth checking rather than assuming:
              # `rooms` and `members` are sibling options of one submodule, and
              # this reads the VALUE of the first while declaring the TYPE of
              # the second. `den.households.<h>.rooms` does not read `members`,
              # so the cycle never closes -- measured by forcing
              # `_module.args.memberships`, which touches every member's rooms
              # for every household.
              rooms = mkOption {
                type = types.listOf (types.enum (lib.attrNames household.rooms));
                default = [ ];
                description = ''
                  The household's rooms that are THIS member's own, by room
                  name. A subset of the household's `rooms` -- the type is that
                  subset, so naming a room that does not exist is refused with
                  the list of the ones that do.

                  Shared rooms need not be listed -- every member gets those.
                '';
              };

              services = mkOption {
                type = types.attrsOf (
                  types.submodule {
                    options.access = mkOption {
                      type = types.str;
                      default = "";
                      description = ''
                        The level this member has in that service, within this
                        household. Empty means the service's own
                        `access.defaultLevel`, so the PRESENCE of the attribute
                        is the request and the level is refinement -- the same
                        reading as `users.<n> = { }` meaning "we need this user
                        here". A service that declares no default makes the
                        level mandatory, as a throw naming its levels.
                      '';
                    };
                  }
                );
                default = { };
                example = lib.literalExpression ''{ vikunja.access = "admin"; }'';
                description = ''
                  What this member may do in each service, WITHIN THIS
                  HOUSEHOLD.

                  Access lives on the membership rather than on the person
                  because it is household-scoped by nature: being an admin of
                  your own house says nothing about a house you are a guest in.
                  The person carries what travels with the human (their
                  aspects); the membership carries what is true of them here.

                  Validated against the household's own service declaration
                  by ../services/access.nix -- an unknown service or level is an
                  evaluation error, not a grant that silently opens nothing.
                '';
              };

              guest = mkOption {
                type = types.bool;
                default = false;
                description = ''
                  Present but not resident. A guest gets the shared spaces and
                  nothing of anyone's own, and is the reason a person cannot
                  simply BE a household member: the same human may be resident
                  in one household and a guest in another.
                '';
              };
            };
          }
        )
      ];
    } den.schema.member;

  # ../base/service.nix owns the rest of the service model -- the schema, the
  # per-host `enable` claim, the `serves` throw and the `serviceHosts` walk. The
  # instance TYPE is applied here, for the same reason `memberType` is: its
  # derived `users` reads this household's members, so it needs the household
  # instance in scope and cannot be built from the schema alone.
  #
  # Read from ./service-libs.nix rather than imported by path: a path import is
  # evaluated per call site, and that file is where every `lib/` service body
  # is pulled in exactly once.
  serviceType = serviceLibs.mkHouseholdServiceType;

  householdType = den.lib.schema.mkInstanceType {
    strict = true;
    extraModules = [
      (
        { config, ... }:
        {
          options.members = mkOption {
            type = types.attrsOf (memberType config);
            default = { };
            description = "The people in this household, keyed by person name.";
          };

          options.serviceGroups = mkOption {
            type = types.attrsOf (
              types.submodule (
                { name, ... }:
                {
                  options = {
                    name = mkOption {
                      type = types.str;
                      readOnly = true;
                      default = name;
                    };

                    displayName = mkOption {
                      type = types.str;
                      default = name;
                    };

                    access = mkOption {
                      type = types.nullOr types.raw;
                      default = null;
                      example = lib.literalExpression ''{ levels = [ "user" "admin" ]; defaultLevel = "user"; }'';
                      description = ''
                        What a grant on this GROUP means. The levels live here
                        rather than on the tag because a tag is a string and has
                        nowhere to put them, and rather than on each member
                        service because the point of the group is that one grant
                        covers all of them.
                      '';
                    };
                  };
                }
              )
            );
            default = { };
            description = ''
              Named groups of services that are granted together. A service
              joins one by setting `group`; this is where the group's own access
              levels are declared.
            '';
          };

          options.services = mkOption {
            type = types.attrsOf (serviceType config);
            default = { };
            description = ''
              What this household provides. Declared here rather than on a host
              so there is one place to see it, and so moving a service between
              machines is moving a `services.<n>.enable` claim rather than the
              declaration.
            '';
          };
        }
      )
    ];
  } den.schema.household;
in
{
  # ── AN OWNED MACHINE IS IN ITS OWNER'S HOUSEHOLD ──
  # A host that states no household takes the one its owner is a member of,
  # so ivy's laptop reaches the household's services as she does. Only when
  # that is unambiguous: an owner in two households has to say which.
  imports = [
    {
      den.schema.host =
        { config, ... }:
        let
          owner = config.owner;
          theirs = lib.filter (h: lib.any (m: m.person.name == owner.name) (lib.attrValues h.members)) (
            lib.attrValues args.config.den.households
          );
        in
        {
          config.household = lib.mkIf (owner != null && lib.length theirs == 1) (
            lib.mkDefault (lib.head theirs)
          );
        };
    }
  ];

  options.den.households = mkOption {
    description = "Places with people in them. Declared in ../households/, one file each.";
    default = { };
    type = types.attrsOf householdType;
  };

  config = {
    # Declares the nesting relation so den walks it.
    #
    # It does NOT make `{ member, ... }` or `{ household, ... }` bindable in
    # arbitrary class content, which an earlier version of this comment claimed.
    # `isEntity` is COMPUTED (den `modules/options.nix`): any `den.schema.<k>`
    # carrying options beyond the collection keys becomes an entity kind, so
    # declaring these options already made `household` and `member` entity
    # kinds. The consequences are the opposite of convenient:
    #
    #   * A BARE entity arg at a scope that cannot bind it makes the whole
    #     aspect SILENTLY INERT -- no error, no content (den
    #     `fx/handlers/bind.nix`; its CI covers this as the "misplaced entity
    #     arg" case). So `{ household, ... }` on an aspect resolved at a scope
    #     with no household in ctx is not a no-op, it is a disappearance.
    #   * `provides.household` / `provides.member` are silently DROPPED, because
    #     `fx/aspect/provide.nix` filters the provides keys against the
    #     entity-kind set. Do not design a `provides.<kind>` fan-out.
    #
    # What DOES bind, and for free: a field NAMED after an entity kind is merged
    # into that entity's scope ctx (den `resolve-entity.nix`,
    # `entityDerivedBindings`). That is why `user.person` makes `{ person, ... }`
    # work at user scopes, and why `host.household` below makes
    # `{ household, ... }` work at host scopes. `host.owner` deliberately does
    # not bind, because the key is the KIND name (`person`), not the field name
    # -- so `owner` is read directly and never destructured.
    # ── A MEMBERSHIP RESOLVES ASPECTS, WHICH IS WHAT PROPAGATION NEEDS ──
    #
    # `parent` alone left `member` a plain record: `isEntity` is computed from
    # whether the SCHEMA ENTRY carries options, and this kind's options are in
    # `memberType`'s `extraModules` instead -- so the entry had only `parent`,
    # the kind was not an entity, and den's `withResolved` (nix/lib/schema.nix)
    # never injected `resolved`. Measured: a member came back with no
    # `resolved` and no `hasAspect`, where `den.households.<h>` -- whose
    # options ARE on the schema entry -- had both.
    #
    # `inherits = [ den.schema.conf ]` is what den's own guide to defining a
    # kind prescribes (templates/ci/modules/public-api/custom-entity-kind.nix,
    # which asserts `hasAspect = true` on exactly this shape). `conf` is the
    # shared base every kind imports, and modules/context/has-aspect.nix
    # defines `hasAspect` on everything that does.
    #
    # ── WHY THE STRUCTURAL QUERY IS ENOUGH, AND NO CONTENT IS FANNED OUT ──
    #
    # `hasAspect` reads `config.resolved`, which each instance computes for
    # itself (`den.lib.resolveEntity`); it does NOT need a `resolve.to` policy
    # dispatching a member scope. That matters, because such a fan-out would
    # make every member a production scope with its own pipeline, and what the
    # consumers here actually ask is structural -- "does this member have the
    # mailbox aspect" -- not "render config for this member".
    #
    # So this is the cheap half of membership propagation: a member now answers
    # `capability.takersOn` and `hasAspect` the way an ACCOUNT does, which is
    # what lets ../services/shares.nix and ../services/mail.nix stop requiring
    # a unix login on the serving host. Adding content per member later is a
    # policy away and needs no change here.
    den.schema.member = {
      parent = "household";
      inherits = [ den.schema.conf ];
    };

    # ── THE PERSON'S ASPECTS, DELIVERED TO THE MEMBERSHIP ──
    #
    # ./person.nix delivers a person's `includes` to their ACCOUNTS, which is
    # the right channel for a shell and an editor: those are things a LOGIN
    # has. It is the wrong channel for a mailbox or a share, and
    # ../services/shares.nix recorded the consequence as a known limit -- "a
    # person with no account on the host that serves the file service
    # contributes no membership ... lifting that means delivering a person's
    # `includes` to the hosts their household is served by".
    #
    # This is that lift, one step better than the note imagined: not to the
    # HOST, which has no person and so could not say whose share it was, but to
    # the MEMBERSHIP -- which is exactly "this human, in this house" and
    # carries the person as a field.
    #
    # `{ member, ... }` is BARE, which is the gate: a bare entity arg dispatches
    # only where that entity binds, so this cannot fire anywhere a person is
    # not a member (and per this file's own header, a bare arg at a scope that
    # cannot bind it makes an aspect silently inert -- here the scope is the
    # member's own, so it binds by construction).
    den.policies.member-person-includes =
      { member, ... }:
      let
        p = member.person or null;
      in
      if p == null then [ ] else map include p.includes;

    den.schema.member.includes = [ den.policies.member-person-includes ];

    # The two host links. Separate options because they are different facts: one
    # is "whose config this host produces", the other is "whose machine it is".
    # secondpc serves the household and is owned by nobody; the MacBook is Ivy's.
    #
    # Both references point DOWN (host -> household -> member -> person, and
    # host -> person), which is the direction rule. Pointing a person or a
    # household back at an entity that resolves aspects is what deadlocked
    # `systemd.services` with `infinite recursion encountered` -- see
    # ./person.nix's header.
    den.schema.host.options = {
      household = mkOption {
        type = types.nullOr types.raw;
        default = null;
        example = lib.literalExpression "den.households.ivymectin";
        description = ''
          The household whose config this host produces -- rooms, members, who
          may reach its services. Null for a host that serves no household.

          Because the field is named after the `household` entity kind, den
          merges it into this host's scope ctx, so `{ household, ... }` binds in
          content resolved here and is inert on a host that sets nothing.

          It is also WHICH SCOPE THIS HOST RESOLVES IN. ./household-scope.nix
          turns a household into a grouping scope on den's fleet pattern, so a
          host that sets this is a CHILD of its household's scope and a sibling
          of the other machines in that house -- which is what `pipe.collect`
          reads. A host that leaves it null keeps `flake-system` as its parent.
          The grouping is partial precisely because this option is nullable.
        '';
      };

      owner = mkOption {
        type = types.nullOr types.raw;
        default = null;
        example = lib.literalExpression "den.persons.ivy";
        description = ''
          The person whose machine this is -- distinct from the household it
          serves. A personal laptop has an owner; a shared service host does
          not.

          Read directly (`host.owner.displayName`), never destructured as
          `{ owner, ... }`: ctx binding keys on the entity KIND name, which is
          `person`, so an `owner` arg would not resolve.
        '';
      };
    };

    den.schema.household.options = {
      displayName = mkOption {
        type = types.str;
        description = "Human-facing name of the household.";
      };

      rooms = mkOption {
        type = types.attrsOf roomType;
        default = { };
        description = ''
          The rooms and spaces in this household. Rooms belong to the PLACE,
          not to a person -- a bedroom does not stop existing because whoever
          sleeps in it moved out -- and members claim a subset of them.
        '';
      };
    };

    # File-level handles, the channel that cannot go inert (see ./person.nix's
    # header for why a promoted field is not enough for anything load-bearing).
    _module.args.households = config.den.households;

    # Every (household, member) pair, flattened -- the shape a consumer that
    # has to walk all memberships at once actually wants, in the spirit of
    # ../hosts/fleet.nix.
    _module.args.memberships = lib.concatLists (
      lib.mapAttrsToList (
        hname: h:
        lib.mapAttrsToList (mname: m: {
          household = hname;
          member = mname;
          inherit (m)
            person
            rooms
            guest
            services
            ;
          # The member's own rooms plus every shared room -- what they can
          # actually reach, which is the question consumers ask.
          reachableRooms = lib.unique (m.rooms ++ (lib.attrNames (lib.filterAttrs (_: r: r.shared) h.rooms)));
        }) h.members
      ) config.den.households
    );
  };
}
