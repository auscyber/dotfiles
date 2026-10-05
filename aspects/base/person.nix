{
  den,
  lib,
  config,
  ...
}:
# A PERSON is a human, as distinct from the accounts they log in with. One human
# here is `ivypierlot` on the darwin boxes, `auscyber` on secondpc/auspc and
# `ivy` on the rest -- but kanidm serves exactly one name through NSS, so the
# identity has to live above the per-host `user` entity.
#
# The people themselves are declared in ../persons/, one file each. This file is
# only the schema and the plumbing.
#
# THE LINK POINTS FROM THE ACCOUNT TO THE PERSON, never the other way:
#
#   den.hosts.<sys>.<host>.users.<account>.person = den.persons.<name>
#
# That direction is load-bearing, not a style choice. The first version had the
# person own a list of user ENTITIES, and it deadlocked: a person's record
# referenced host entities, every user entity derived its person by searching
# those lists, and so resolving any user forced the registry which forced the
# entities which forced resolution. `systemd.services` on secondpc came back
# `infinite recursion encountered`, and detaching the include policies and the
# whole access collector did not clear it, because the cycle was in the
# reference direction itself rather than in any one consumer.
#
# Pointing the other way the graph is acyclic: an account names its person, the
# registry names nothing, and anything that needs "which accounts belong to this
# person" asks the accounts rather than the person.
#
# A person is also NOT a parented sub-entity (`parent = "host"`): they span
# hosts, and standalone `den.homes` entries have no host to nest under at all.
# ./anyuser.nix documents the same problem for `anyUser`.
#
# What this collection gives up, deliberately: `resolvedCtxModule` (which den's
# host.nix/home.nix pass in `extraModules` to attach the `resolved` field) is
# not exported through `den.lib`, so a person carries identity and its own
# options but gets no aspect resolution of its own -- `den.persons.<n>` has no
# `den.aspects.<n>` lookup. Its `includes` are delivered to its ACCOUNTS instead,
# which is what they are for.
let
  inherit (lib) mkOption types;
  inherit (den.lib.policy) include mkPolicy;

in
{
  options.den.persons = mkOption {
    description = "The humans in the fleet. Declared in ../persons/, one file each.";
    default = { };
    type = types.attrsOf (
      den.lib.schema.mkInstanceType {
        strict = true;
        # ── THE THREE NAMES DEFAULT OFF EACH OTHER ──
        #
        # Declared HERE and not as `default` on the options, because each one
        # falls back to something only the INSTANCE knows: the entity's own
        # key, or another of its fields. `den.schema.person.options` is a plain
        # attrset with no instance in scope, so a default there could only be a
        # constant.
        #
        # `mkDefault` rather than a plain value, so a person stating any of
        # them wins without `mkForce`.
        extraModules = [
          (
            { name, config, ... }:
            {
              preferredName = lib.mkDefault name;
              displayName = lib.mkDefault config.preferredName;
              kanidm.name = lib.mkDefault name;
            }
          )
        ];
      } den.schema.person
    );
  };

  config = {
    den.schema.person.options = {
      # ── THREE NAMES, BECAUSE A HUMAN HAS THREE ──
      #
      # The KEY is an account name, and it has to be: kanidm keys its persons
      # by it, refuses a non-lowercase one, and the live instance already has
      # `auscyber` with enrolled passkeys. The human it belongs to is Ivy. Those
      # are different facts and conflating them is why "the person is ivy, not
      # auscyber" is a sentence anyone had to say.
      #
      #   name (the key)   what kanidm, NSS and the unix account call them
      #   preferredName    what a person is called -- "Ivy"
      #   displayName      what kanidm shows -- "Ivy Pierlot"
      #   legalName        kanidm `legalname`, for the one or two things that
      #                    need it
      #
      # `kanidm.name` below decouples the first from the key, so the repo can
      # rename the entity to `ivy` without renaming the live account.
      preferredName = mkOption {
        type = types.str;
        defaultText = "the person's key";
        description = ''
          What this person is CALLED, as against what their account is named.
          Defaults to the key, which is right for a person whose account name
          is already their name and wrong for everyone else.
        '';
      };

      displayName = mkOption {
        type = types.str;
        defaultText = "preferredName";
        description = ''
          kanidm `displayname`, and the git/jj `user.name`. Defaults to
          `preferredName`, so a person declared with nothing but a key still
          provisions -- a mandatory `displayName` is why
          ../hosts/secondpc/sso.nix carried a hand-written copy of this person
          instead of reading the registry, and why ../persons/others.nix could
          not be provisioned at all.
        '';
      };

      legalName = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "kanidm `legalname`. Left null unless something needs it.";
      };

      mailAddresses = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "kanidm `mail`. The first is the primary -- kanidm's rule, not a convention here.";
      };

      # ── MAIL, WHICH IS THE ADDRESSES PLUS WHAT THEY CANNOT SAY ──
      #
      # WHICH addresses are local is NOT declared here: it follows from the
      # domains the mailserver hosts (../services/mail.nix's `hostedDomains`),
      # so an address in one of them is a mailbox and every other address a
      # person holds stays an external identity. A second `localAddresses`
      # list would be the same fact twice, able to disagree -- the thing this
      # tree keeps removing.
      #
      # These are only the facts the address list cannot carry.
      mail = {
        enable = mkOption {
          type = types.bool;
          default = true;
          description = ''
            Whether this person gets a MAILBOX on whatever host serves the
            household's mail. Defaults true because holding a local address is
            already the opt-in: a person with only external addresses gets
            nothing either way. False for someone whose local address should be
            an alias or a forward rather than a mailbox of its own.
          '';
        };

        primary = mkOption {
          type = types.nullOr types.str;
          default = null;
          example = "ivy@ivymect.in";
          description = ''
            Which hosted address IS the mailbox; the rest become aliases that
            route to it.

            A FIELD RATHER THAN "the first one", for two independent reasons.
            `mailAddresses`'s order already means something else -- the first
            entry is kanidm's primary (kanidm's rule, see above), and today
            that is an external address. And the mailbox key is the maildir
            PATH, so re-ordering that list for a kanidm reason would rename the
            mailbox and orphan every message in it.

            Null is fine while a person has at most one hosted address;
            ../services/mail.nix throws rather than guessing once there are
            two.
          '';
        };

        extraAliases = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = "Addresses that route to this person's mailbox without being identities of theirs.";
        };

        catchAll = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = "Hosted domains whose otherwise-undeliverable mail lands in this mailbox.";
        };

        quota = mkOption {
          type = types.nullOr types.str;
          default = null;
          example = "20G";
          description = "Mailbox quota. Null takes the server's default.";
        };

        sendOnly = mkOption {
          type = types.bool;
          default = false;
          description = "An address that may send but rejects incoming mail.";
        };
      };

      # ── THE KEYS A HUMAN AUTHENTICATES WITH ──
      #
      # A LIST, and merged rather than replaced, because the private halves
      # live in a password manager and there is more than one of them -- a
      # yubikey, a laptop, the 1Password agent. ../security/_main-key.nix held
      # exactly one of these as a file-level constant attached to every account
      # on every machine, which could express neither "Ivy has three keys" nor
      # "this one is revoked".
      #
      # DISTINCT FROM `hostPublicKey`, and the distinction is the same one
      # ../security/main-ssh-key.nix already draws: `hostPublicKey` is an
      # ON-SYSTEM identity, the thing agenix-rekey encrypts a machine's or an
      # account's secrets TO, and it is per machine. These are OFF-SYSTEM
      # credentials the human authenticates WITH, and they are per human --
      # which is why they belong here and not on an account.
      #
      # WHERE THEY GO: every account that names this person
      # (../security/main-ssh-key.nix, delivered through `includes` to each of
      # them), so `ivypierlot` on the mac and `auscyber` on auspc trust the
      # same set without either machine naming a key.
      #
      # WHERE THEY DO NOT GO, YET: the agenix recipient set. Adding a key there
      # changes what every rekeyed secret is encrypted to, which needs
      # `nix run .#rekey` -- and ../../CLAUDE.md freezes that while the master
      # key is mid-recovery.
      sshKeys = mkOption {
        type = types.listOf types.str;
        default = [ ];
        example = lib.literalExpression ''[ "ssh-ed25519 AAAAC3Nza... ivy@1password" ]'';
        description = ''
          Public halves of the SSH keys this person authenticates with. Merged
          onto every account that names them, for both login and pam_rssh
          sudo.
        '';
      };

      kanidm = {
        name = mkOption {
          type = types.str;
          defaultText = "the person's key";
          description = ''
            The kanidm account name, which the key defaults to and which is
            NOT the same question. kanidm keys its persons by this and a
            rename there is a delete-and-recreate under `autoRemove` -- every
            enrolled passkey with it -- so pinning it here is what lets the
            entity be renamed for the humans reading the repo.
          '';
        };

        groups = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            RAW kanidm groups this person joins, for the ones that are not a
            service gate -- `idm_admins`, `media-users`. Service access is
            derived from the household membership instead.
          '';
        };
      };

      includes = mkOption {
        type = types.listOf types.raw;
        default = [ ];
        example = lib.literalExpression "[ den.aspects.fish den.aspects.neovim ]";
        description = ''
          Aspects this person includes. A human's shell, editor and dotfiles
          belong to the HUMAN, not to each machine they happen to have an
          account on -- and so does "Ivy listens to Tidal".

          This is deliberately an ASPECT LIST and not a per-service settings
          attrset. An earlier version had `services.<svc>.settings` as raw
          options on the person; that is the wrong primitive in this repo --
          aspects compose, carry their own secrets and class content, and are
          how everything else here is expressed, whereas an options blob on a
          person can only be read by something that already knows its shape.
          So "Ivy wants Tidal in Music Assistant" is `den.aspects.tidal` in
          this list, not a settings tree here.

          Delivered as an include EFFECT from a policy on
          `den.schema.user.includes`, not by setting `includes` on the entity:
          an entity INSTANCE has no `includes` option (checked -- it came back
          with only `_identity`/`_identityKeys`/`name`/`id_hash` and its own
          fields), only the schema does, and the schema applies to every user
          at once. The policy's gate is what makes it per-person. Same
          mechanism ../framework/roles.nix uses, and for the same reason: an
          include effect is re-dispatched through den's aspect pipeline, so a
          delivered aspect's own `provides.to-users` still fans out, which a
          plain conditional attrset would silently drop.
        '';
      };

      provisionKanidm = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether this person exists in kanidm at all.

          READ BY ../services/kanidm.nix, which provisions one kanidm person
          per entry here. It was declared and read by nothing until then:
          ../hosts/secondpc/sso.nix listed `persons.auscyber` by hand, with
          this person's display name and mail address written out a second
          time, so the registry described people the identity provider had
          never heard of.
        '';
      };

      # Not every person is a unix user. SSO-only is the common case: a person
      # with `enableUnix = false` gets a kanidm identity, group memberships and
      # OAuth2 access, and no POSIX attributes -- so NSS never serves them and
      # PAM cannot admit them.
      enableUnix = mkOption {
        type = types.bool;
        default = false;
        description = ''
          POSIX-extend this person, so kanidm serves them through NSS and PAM.
          Off by default: an SSO-only person needs none of it.

          One-way. kanidm has no un-extend API, so setting this back to false
          is a NO-OP against an already-extended account rather than a removal,
          and `gidNumber` is baked into file ownership from the moment it lands.
        '';
      };

      gidNumber = mkOption {
        # kanidm uses ONE number per posix person: it is the uid AND the gid of
        # the user-private group kanidm synthesises. Do not model them
        # separately. The bound is kanidm's `apply_gidnumber`
        # (server/lib/src/plugins/gidnumber.rs); the sub-bands it rejects inside
        # this range are checked where the number is consumed.
        type = types.nullOr (types.ints.between 1000 524287);
        default = null;
        description = ''
          POSIX uid, and the gid of this person's user-private group. Null lets
          kanidm derive one from the entry uuid into 0x70000000-0x7fffffff --
          stable per instance, meaningless across machines, which defeats the
          point of sharing a filesystem.
        '';
      };

      loginShell = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "kanidm `loginshell`.";
      };

      # ── ONE PLAINTEXT PER HUMAN, NAMED ONCE ──
      #
      # A `{ scope; key; }` pair and not a bare key, because the secret does
      # not live in one predetermined aspect: Ivy's is `user-pwd/ivy-password`
      # (../security/user-pwd.nix), which predates all of this and cannot move
      # -- renaming it means regenerating it, and ../../CLAUDE.md freezes that
      # while the master key is mid-recovery.
      #
      # WHAT READS IT: ../services/samba-users.nix, which mints this person's
      # NT hash from it (MD4 of the UTF-16LE plaintext), so the SMB credential
      # and the human's real password stay one credential. It used to be
      # pinned in a `pins` table in that file, keyed by person -- a second
      # place to say who somebody is.
      #
      # WHAT DOES NOT, AND WHY: kanidm's own unix password. kanidm-provision
      # cannot set one (its schema has no POSIX fields at the pinned version),
      # so it stays `kanidm person posix set-password <account>` by hand, typed
      # from this same secret. That is the one step that keeps SMB, PAM and SSO
      # on one password, and nothing here can automate it yet.
      passwordSecret = mkOption {
        type = types.nullOr (
          types.submodule {
            options = {
              scope = mkOption {
                type = types.str;
                description = "The `age.scoped.<scope>` the secret lives in.";
              };
              key = mkOption {
                type = types.str;
                description = "Its key within that scope.";
              };
            };
          }
        );
        default = null;
        example = lib.literalExpression ''{ scope = "user-pwd"; key = "ivy-password"; }'';
        description = ''
          Where this person's PLAINTEXT password lives, as a scope and a key
          rather than a path -- only the aspect that owns the scope can
          resolve `scoped.<scope>.secrets.<key>.path`.

          Null means this person has no shared password, and anything needing
          one mints its own (../services/samba-users.nix does exactly that).
        '';
      };
    };

    # Deliver a person's `includes` to every account that names them.
    #
    # Reads `user.person` directly -- no index, no search. That is the whole
    # payoff of the account->person direction: there is nothing to invert and so
    # nothing to recurse through.
    den.policies.person-includes =
      {
        user ? { },
        ...
      }:
      let
        p = user.person or null;
      in
      if p == null then [ ] else map include p.includes;

    den.policies.person-includes-home =
      {
        home ? { },
        ...
      }:
      let
        p = home.person or null;
      in
      if p == null then [ ] else map include p.includes;

    # `person` is nullable so a service account or a throwaway VM user
    # (../hosts/pentestvm.nix's `admin`, ../hosts/wsl-nixos.nix's `nixos`) is
    # not an eval error -- those accounts are not anybody's identity.
    den.schema.user = {
      includes = [ den.policies.person-includes ];
      options.person = mkOption {
        type = types.nullOr types.raw;
        default = null;
        example = lib.literalExpression "den.persons.ivy";
        description = "The person this account belongs to, referred to directly.";
      };
    };

    den.schema.home = {
      includes = [ den.policies.person-includes-home ];
      options.person = mkOption {
        type = types.nullOr types.raw;
        default = null;
        example = lib.literalExpression "den.persons.ivy";
        description = "The person this standalone home belongs to, referred to directly.";
      };
    };

    # The channel that cannot go inert: aspect FILES read the registry here,
    # class content binds `{ person, ... }`. ../hosts/fleet.nix is the model.
    _module.args.persons = config.den.persons;
  };
}
