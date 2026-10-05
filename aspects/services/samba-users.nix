{
  den,
  lib,
  capability,
  caps,
  ...
}:
# SMB AUTHENTICATION IS AN ASPECT A PERSON INCLUDES, which is the model
# ./shares.nix carries too: the include edge IS the opt-in, there is no member
# list, and the resolved set lands in a plain NixOS option (`config.sambaUsers`)
# that the backend -- ../hosts/secondpc/samba.nix -- reads.
#
#   den.persons.<p>.includes = [ den.aspects.samba-user ];
#
# and that person gets an NT-hash secret of their own plus a passdb entry on
# every host that both serves samba and either has an account of theirs or has
# them as a member of the household it serves.
#
# WHY A SEPARATE CREDENTIAL AT ALL, since kanidm is the identity provider: SMB
# authentication needs an NT hash (MD4 of the UTF-16LE password) and kanidm's
# LDAP gateway is read-only with none of the Samba schema, so `ldapsam` against
# it is not a backend at any version. ../hosts/secondpc/samba.nix's header has
# the long version and kanidm/kanidm#2627 is the upstream epic.
#
# WHY THIS IS SEPARABLE FROM UNIX ACCOUNTS, which is the point of the split:
# kanidm serves the identity through NSS (sso.nix's `enablePam`), and the NT hash
# supplies only the authentication. Neither needs `users.users.<n>` to be
# declared on the host -- a samba user is a person plus a hash, and nothing else.
# What it DOES still need is that NSS can resolve the name at all, because
# `pdbedit -a` goes through getpwnam(); for a kanidm-only person that means the
# person is POSIX-extended in kanidm (`kanidm person posix set <p>`), which
# kanidm-provision's schema cannot express and so is a one-off by hand.
#
# ── THE MIGRATION THIS FILE OWED IS DONE ──
#
# It was a quirk: `den.quirks.samba-user` pooling emissions pushed up with
# `pipe.expose` from each includer's USER scope, a `den.policies.samba-user-owner`
# on `den.schema.user.includes` stamping each emission with the person whose
# scope it resolved at, and a `norm` applying defaults with `or`. All three are
# gone, replaced by `den.capabilities.samba-user` below -- the same three wins
# ./shares.nix's own header records for the same move:
#
#   * THE PERSON IS DERIVED FROM WHERE THE TAKE IS FOUND, not stamped onto an
#     emission. `capability.takersOn` works on any entity with `hasAspect`, so
#     the asker is the account or the membership, and `pipe.expose` -- which
#     existed only to cross the user-scope-to-host-scope edge -- is unnecessary.
#   * ITS FIELDS ARE TYPED, by the `taker` module, where `norm` applied defaults
#     with `or` and checked nothing.
#   * AND THERE IS NOTHING LEFT TO DISAGREE. The quirk emitted once per includer,
#     so a field-by-field `pick` had to throw when two emissions for one person
#     conflicted. `capability.contributions` reads one declaration off one
#     aspect, so a person asked twice -- once as an account, once as a member --
#     answers identically by construction and the duplicate collapses.
#
# ── AND THAT IS WHAT LIFTS THE LIMIT THIS HEADER USED TO CARRY ──
#
# The old note said this resolved only on a host where the person has a unix
# account, because a person's `includes` reach their ACCOUNTS. ./shares.nix and
# ./mail.nix had already dropped that limit by asking the household's members
# too -- a membership is an aspect-resolving entity (../base/household.nix) --
# and a quirk had no "ask this member" form. A capability does, so this file now
# asks both and a person in the house needs no login on the server.
#
# ⚠ WHAT IT DOES NOT LIFT, AND THE ONE THING TO KNOW BEFORE ADDING A MEMBER:
# each samba user needs an NT-hash secret, and a NEW one cannot be materialised
# while the master key is mid-recovery (../../CLAUDE.md forbids `gen-secrets`).
# So a member added here who has no account gets into `share-<n>` and into
# samba's `valid users` with no hash behind them -- SMB auth then fails at
# runtime with nothing wrong at evaluation. The mechanism is ready; the person
# is one `gen-secrets` away, not one line away.
let
  # SECRET NAMES PINNED TO WHAT IS ALREADY GENERATED. `auscyber`'s NT hash
  # predates this model: it exists as the flat `ivy-nt-hash`
  # (secrets/generated/secondpc/ivy-nt-hash.age), minted from the `ivy-password`
  # intermediary in ../security/user-pwd.nix -- which is also the human's real
  # password, so the SMB login and the SSO login stay one credential.
  #
  # Deriving the name from the person instead would strand that secret and need
  # a `nix run .#gen-secrets` to materialise the replacement, which is exactly
  # what cannot be run while the master key is mid-recovery. So it is pinned and
  # the generated secret set is unchanged by this file.
  #
  # ONLY THE HASH IS PINNED. The PASSWORD half lives on the person
  # (`den.persons.<p>.passwordSecret`, ../base/person.nix): one human, one
  # plaintext, named where the human is declared rather than in a table here
  # keyed by their account name.
  #
  # A NEW person needs no entry: they get `samba-nt-hash-<p>` minted from a
  # random `samba-password-<p>` (see ../hosts/secondpc/samba.nix), because there
  # is no reason for a second person's SMB credential to be their unix one.
  ntHashPins = {
    auscyber = "ivy-nt-hash";
  };
in
{
  # ── THE CAPABILITY, DECLARED WHERE IT IS OWNED ──
  #
  # Here rather than in ../base/capability-types.nix, for the reason the rest of
  # this tree's capabilities moved out of that file: "what a samba user is" is
  # this aspect's own fact, and a definition sitting in a file that knows nothing
  # about samba makes ownership and editing odd.
  den.capabilities.samba-user = {
    meta.description = "A person with SMB authentication, declared by including den.aspects.samba-user and had by whoever includes it.";
    includes = [ caps.carried ];

    # NOT ROUTED, and no longer said here: nothing gives `samba-user` and
    # nothing ever will -- the take is a marker and the collector below reads it
    # with `takersOn`/`contributions` rather than resolving a link. That is now
    # DERIVED, because this capability narrows no `address`
    # (../base/capability.nix's `isRouted`), so the flag this used to set was a
    # second copy of it. See the note where it was declared.

    # EVERY FIELD IS AN OVERRIDE, hence `nullOr` with a null default rather than
    # a real one. The honest default for each of these is a function of the
    # PERSON -- their own `passwordSecret`, or a name derived from theirs -- and
    # a taker does not know which person it was found at. So null means "derive
    # it from the person", the collector does that derivation once, and a
    # non-null value here is a deliberate exception.
    taker.options = {
      # Scope the plaintext password lives in, as an `age.scoped.<scope>` name.
      passwordScope = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };

      passwordSecret = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };

      ntHashSecret = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };
    };
  };

  # THE OPT-IN. A take and nothing else: a person's `includes` are delivered to
  # every account they have and to their membership, so anything in here beyond
  # the take would land on machines that serve no files.
  den.aspects.samba-user.takes.samba-user = { };

  # THE COLLECTOR. It resolves the askers into a plain option and does nothing
  # else, so the backend cannot disagree with it about who a samba user is.
  den.aspects.samba-users = {
    nixos =
      {
        host ? { },
        ...
      }:
      let
        # THE TWO KINDS OF ASKER, exactly as ./shares.nix asks them. An account
        # is a person's login on this box; a membership is "this human, in this
        # house" -- which is what lets a person who has no login here still be
        # given SMB authentication by the machine serving their household.
        #
        # `host.household` is null on a host that serves no household, which is
        # the honest value and yields nothing. The second `or` catches the
        # select-on-null; the first cannot, because `household` is a declared
        # option whose default IS null and `or` fires only on a MISSING
        # attribute (../../CLAUDE.md).
        accounts = lib.attrValues (host.users or { });
        members = lib.attrValues ((host.household or null).members or { });

        # ONE ANSWER PER ASKER: whether it takes this, whose it is, and what it
        # overrode. The person ENTITY rather than the name, because the
        # derivation below reads `passwordSecret` off it.
        ask = p: e: {
          person = p;
          takers = capability.takersOn e "samba-user";
          contributed = capability.contributions e "samba-user";
        };

        asked =
          map (u: ask (u.person or null) u) accounts
          # A membership always has a person -- ../base/household.nix throws on
          # a member naming one that is not declared -- so there is no null case
          # to carry here.
          ++ map (m: ask m.person m) members;

        # An account with no person behind it cannot be a samba user, and that
        # is a throw rather than a skip: the old quirk threw on exactly this
        # ("a samba user is a person, not an account"), and silently dropping it
        # would make `den.aspects.samba-user` on a personless account look like
        # it worked.
        taking = map (
          a:
          if a.person != null then
            a
          else
            throw "den.aspects.samba-user: an account on ${host.name or "this host"} takes `samba-user` but names no person (`users.<n>.person`). A samba user is a person, not an account -- ../base/person.nix."
        ) (lib.filter (a: a.takers != { }) asked);

        # The single override for a field, across however many takers answered.
        # Nulls are dropped first, so "declared nothing" and "did not declare"
        # are the same answer -- which is what the null default means.
        override =
          a: field:
          let
            vals = lib.unique (lib.filter (v: v != null) (lib.attrValues (a.contributed.${field} or { })));
          in
          if vals == [ ] then null else lib.head vals;

        entryFor =
          a:
          let
            # The KANIDM/NSS name. `pdbedit -a` resolves it through getpwnam and
            # the passdb entry is keyed by it, so this is the ACCOUNT name and
            # not the human's -- which are different strings for Ivy
            # (../base/person.nix).
            person = a.person.kanidm.name;

            # The one fact that is the person's rather than this host's: where
            # their plaintext lives. `or null` AND an explicit null test,
            # because the option exists with a null default on a person who has
            # not declared one.
            declared = a.person.passwordSecret or null;
          in
          lib.nameValuePair person {
            inherit person;
            passwordScope =
              let
                o = override a "passwordScope";
              in
              if o != null then
                o
              else if declared == null then
                null
              else
                declared.scope;
            passwordSecret =
              let
                o = override a "passwordSecret";
              in
              if o != null then
                o
              else if declared == null then
                "samba-password-${person}"
              else
                declared.key;
            ntHashSecret =
              let
                o = override a "ntHashSecret";
              in
              if o != null then o else ntHashPins.${person} or "samba-nt-hash-${person}";
          };

        # KEYED BY PERSON, so a human who is both an account here and a member
        # of the household this box serves is one entry. They are asked twice
        # and answer identically -- `contributions` reads one declaration off
        # one aspect -- so the duplicate collapses rather than needing the
        # field-by-field conflict throw the quirk required.
        resolved = lib.listToAttrs (map entryFor taking);
      in
      {
        # Declared through `imports` because the rest of this block is bare
        # config, and one module may not carry `options` beside bare config keys
        # -- ../services/nextcloud.nix's `nextcloud.sharedDir` does the same.
        imports = [
          {
            options.sambaUsers = lib.mkOption {
              type = lib.types.raw;
              readOnly = true;
              default = resolved;
              description = "People this host gives SMB authentication to, keyed by person, resolved from every account and household membership here that takes `samba-user`.";
            };
          }
        ];
      };
  };

  # Who has it. One line, and it is the whole grant -- the hash, the passdb entry
  # and the access all follow from it.
  den.persons.ivy.includes = [ den.aspects.samba-user ];
}
