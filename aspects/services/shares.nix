{
  den,
  lib,
  caps,
  ...
}:
# A SHARE is an ASPECT, and who has it is whoever INCLUDES it.
#
# "shared folders for example with nextcloud or samba, we can declare them say
# this is a single users, or also say shared access to them all" -- so the two
# cases are one person's folder and the whole house's folder, and the thing that
# distinguishes them is how many includers the share has. One person includes
# `den.aspects.photos` and it is theirs; four people include it and it is
# shared. There is no member list anywhere, which is the point: "drop a person
# and it drops their spaces" is then not a cleanup step, it is the absence of a
# line of config.
#
# WHY NOT AN OPTION ON THE HOUSEHOLD OR THE PERSON. That was built first and
# backed out. A `shares.<n>.members` list is a second place to say who somebody
# is, it has to be validated against the person registry, and removing a person
# leaves a dangling name in it -- exactly the failure ../base/person.nix exists
# to prevent for accounts. An aspect needs none of that: the include edge IS the
# membership, the aspect graph already knows how to compose and deliver it, and
# a person's `includes` already fan out to their accounts.
#
# ── A SHARE IS A CAPABILITY A FILE SERVICE GIVES, NOT A QUIRK POOL ──
#
# This was a quirk (`den.quirks.share`), a policy on `den.schema.user.includes`
# that stamped each emission with the person whose scope it resolved at, and a
# resolved `config.shares` NixOS option for the backends to read. All three are
# gone, replaced by `den.capabilities.share` (../base/capability-types.nix):
#
#   * A SHARE NAMES ITSELF ONCE. A take is keyed by the TAKING ASPECT, so
#     `den.aspects.photos` is the name -- where the quirk pool carried no
#     aspect identity and every share had to repeat its own name in its
#     emission with nothing checking the two agreed.
#   * ITS FIELDS ARE TYPED, by the `taker` module the capability declares, in
#     place of a `norm` that applied defaults with `or` and checked nothing.
#   * EVERY INCLUDER SEES THE SAME SHARE BY CONSTRUCTION. The quirk emitted once
#     per includer, so a field-by-field `pick` had to throw when two emissions
#     disagreed; `den.lib.capability.contributions` reads one declaration off one
#     aspect, so there is nothing left to disagree.
#
# ── MEMBERSHIP IS ASKED OF THE ACCOUNT, AND THAT IS WHY `pipe.expose` IS GONE ──
#
# `den.lib.capability.takersOn` answers "which aspects here take this", and it takes any
# entity with `hasAspect` -- so the entity to ask is the ACCOUNT, not the host.
# A person's `includes` are delivered to their accounts (../base/person.nix), so
# a share aspect resolves at the USER scope; the `host` a class body is handed
# carries den's PROJECTED `hasAspect`, which answers "is this delivered into
# THIS scope" (den `nix/lib/aspects/fx/policy/schema.nix`, on the id_hash
# re-key), so `takersOn host "share"` comes back EMPTY and every share silently
# vanishes.
#
# Asking each account is both the correct question and the one that carries the
# owner: the account that answers IS a member, so nothing has to be stamped onto
# an emission and pushed up a scope.
#
# ── THAT LIMIT IS LIFTED, AND THE MEMBERSHIP IS WHAT LIFTED IT ──
#
# This header used to record: "a person's `includes` reach their ACCOUNTS. A
# person with no account on the host that serves the file service therefore
# contributes no membership ... lifting that means delivering a person's
# `includes` to the hosts their household is served by."
#
# It went to the MEMBERSHIP instead, which is the better answer for the reason
# the old one was awkward: a host has no person, so a host-level delivery could
# say "this box has the photos share" but not whose it was.
# `den.households.<h>.members.<p>` is an aspect-resolving entity now
# (../base/household.nix), carrying the person as a field -- so it answers
# `takersOn` exactly as an account does and nothing has to be stamped onto an
# emission.
#
# A share is therefore shareable with anyone in the house, with or without a
# unix login on the server.
let
  # ── THE RESOLVED SET, AS A FUNCTION OF THE HOST ──
  #
  # Hoisted to file level: a function of the aspect graph and the host's
  # accounts, so it needs no host fixpoint and no NixOS option to carry it --
  # which is what `config.shares` was. ../services/proxy-nginx.nix's
  # `entriesFor` is the same shape, and ../../docs/capabilities.md has the
  # measurement behind the rule.
  sharesFor =
    host:
    let
      accounts = lib.attrValues (host.users or { });

      # ── AND THE HOUSEHOLD'S MEMBERS, WHICH IS WHAT LIFTS THE OLD LIMIT ──
      #
      # The header used to end with a limit: "a person with no account on the
      # host that serves the file service therefore contributes no membership
      # ... lifting that means delivering a person's `includes` to the hosts
      # their household is served by". That is done, and one step better than
      # the note imagined -- not to the HOST, which has no person and so could
      # not say whose share it was, but to the MEMBERSHIP, which is exactly
      # "this human, in this house" (../base/household.nix).
      #
      # So a member answers `takersOn` the way an account does, and a person in
      # the house gets their shares whether or not they have a unix login here.
      # `host.household` is null on a host that serves no household, which is
      # the honest value and yields nothing.
      members = lib.attrValues ((host.household or null).members or { });

      # ONE ANSWER PER ASKER, whether that asker is an account or a membership:
      # which shares it has, and who it belongs to.
      #
      # The KANIDM name in both cases: it ends up in
      # `provision.groups.share-<n>.members` and in samba's `valid users`, both
      # of which resolve ACCOUNTS. The person's key identifies the human in
      # this repo and is not always the same string (../base/person.nix).
      #
      # A person who is BOTH a member here and has an account here is asked
      # twice and answers the same -- `membersOf` is `lib.unique` and the taker
      # fold is a union, so the duplicate collapses rather than producing two
      # entries for one human.
      asked =
        map (u: {
          person = if (u.person or null) == null then null else u.person.kanidm.name;
          takers = den.lib.capability.takersOn u "share";
          contributed = den.lib.capability.contributions u "share";
        }) accounts
        ++ map (m: {
          # A membership always has a person -- ../base/household.nix throws on
          # a member naming one that is not declared -- so there is no null
          # case to carry here.
          person = m.person.kanidm.name;
          takers = den.lib.capability.takersOn m "share";
          contributed = den.lib.capability.contributions m "share";
        }) members;

      takers = lib.foldl' (acc: a: acc // a.takers) { } asked;

      # FIELD BY FIELD, because each account's fold holds only the shares that
      # account has. A union rather than a merge, and only because every key is
      # `each`: `contributions` reads each value off the TAKING ASPECT, so two
      # accounts that have the same share read one declaration and cannot
      # disagree. A `union` or `exclusive` key would need folding across
      # accounts instead, which is why ../base/capability-types.nix's `share`
      # declares none.
      contributed = lib.zipAttrsWith (_: vs: lib.foldl' (acc: v: acc // v) { } vs) (
        map (a: a.contributed) asked
      );

      membersOf =
        name:
        lib.unique (
          lib.concatMap (a: lib.optional (a.person != null && a.takers ? ${name}) a.person) asked
        );

      resolve =
        name: _aspect:
        let
          shown = contributed.displayName.${name};
        in
        {
          inherit name;
          displayName = if shown == null then name else shown;
          quota = contributed.quota.${name};
          permissions = contributed.permissions.${name};
          # For a filesystem backend. groupfolders has NO say in where its bytes
          # live (they go in `appdata_<instanceid>/__groupfolders/<id>`), so
          # ./nextcloud.nix ignores this; samba falls back to its share root.
          path = contributed.path.${name};
          members = membersOf name;
          # The kanidm group that carries it. One namespace, so user_oidc's
          # group whitelist in ./nextcloud.nix can be a single anchored regex
          # and nothing else kanidm provisions is in its reach.
          group = "share-${name}";
        };
    in
    lib.mapAttrs resolve takers;
in
{
  # THE CHANNEL THE BACKENDS READ, beside `capability` itself: a function of the
  # host rather than a NixOS option, so ./nextcloud.nix and
  # ../hosts/secondpc/samba.nix cannot disagree about what a share is and
  # neither has to force a host fixpoint to find out. ../base/person.nix's
  # `persons` and ../hosts/fleet.nix's `fleet` are the same channel.
  _module.args.sharesFor = sharesFor;

  den.aspects.shares = {
    # For `services.kanidm.provision.*` to exist on the host at all.
    includes = [ den.aspects.gateway ];

    # ── WHAT GIVES `share`, so a take has something to resolve against ──
    #
    # A pure contribution surface, exactly as ../services/proxy-nginx.nix's
    # `gives.proxy` is: the capability is the declaration surface and this is
    # the one thing on a host that turns a take into config -- the kanidm group
    # below, which both file backends then gate on.
    gives =
      { self, ... }:
      {
        share = _: {
          carrier = self.name;
        };
      };

    # A share is served by the host whose accounts have it. It is not borrowed
    # across machines, and the member set would mean nothing if it were.
    reach = "host";

    nixos =
      {
        host ? { },
        ...
      }:
      {
        # Through `nixos` and NOT the `kanidmGroups` class forward, for one
        # reason: this needs `host` to know whose accounts have what, and class
        # content taking arguments is a FUNCTION -- which den evaluates as a
        # module, merging the module system's own `warnings = [ ]` into an
        # `attrsOf submodule`. ./kanidm.nix hit exactly that
        # (`gateway.oidcClients.warnings is not of type submodule`) and
        # ./proxy-nginx.nix is written this same way for this same reason.
        #
        # `nixos` is also where ./access.nix writes its groups, and for the same
        # safety: the NixOS option system merges `provision.groups` by
        # attribute, so this cannot eat sso.nix's `media-users` or gateway's
        # `svc-*` the way one-level class merging would.
        services.kanidm.provision.groups = lib.mapAttrs' (
          _: s: lib.nameValuePair s.group { inherit (s) members; }
        ) (sharesFor host);
      };
  };

  # ── THE CAPABILITY, DECLARED WHERE THE SHARES ARE RESOLVED ──
  #
  # Same move as `postgres` and `dns`: this file is what turns a share into a
  # groupfolder and an SMB path, so it is where a change to what a share IS
  # belongs.
  # ── A SHARED FOLDER, WHERE THE TAKER *IS* THE THING ASKED FOR ──
  # Third contribution surface after ./postgres and ./home-assistant, and the
  # one that puts the identity in the right place: a share aspect's whole
  den.capabilities.share = {
    meta.description = "A shared folder, declared by the aspect that is the share and had by whoever includes it.";
    includes = [ caps.carried ];

    taker.options = {
      # NULL RATHER THAN THE NAME, because the fallback is the resolver's:
      # ../services/shares.nix fills it from the taker's own name, and there
      # is no spelling of "the aspect's name" available here.
      displayName = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };

      # `null` IS NO CAP, and each backend has its own word for that:
      # groupfolders takes the literal string `unlimited` (Command/Quota.php)
      quota = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };

      # groupfolders' OWN WORDS (Command/Group.php `PERMISSION_VALUES`), not a
      # vocabulary invented here, so a backend reads the ones it understands:
      permissions = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [
          "read"
          "write"
          "share"
          "delete"
        ];
      };

      # WHERE THE BYTES LIVE, for a filesystem backend. groupfolders has no
      # say in it -- they go under `appdata_<instanceid>/__groupfolders/<id>`
      path = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };

      # NO `members`. Membership is the include edge and nothing else: a list
      # here would be a second place to say who somebody is, which is the
    };
  };
}
