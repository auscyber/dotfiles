{
  den,
  lib,
  caps,
  ...
}:
# What used to be the mosquitto add-on.
let
  # MQTT's registered port. Declared here because this aspect is what listens
  # on it, and furnished below so the firewall and every consumer resolve it
  # rather than sharing a constant. It lived in a `_ports.nix` read by a path
  # import until the capability made that unnecessary.
  mqttPort = 1883;

  # One socket, named by two capabilities (`mqtt` narrows `tcp`), so one
  # statement of what speaks it.
  brokerPortDescription = "The broker's plain MQTT listener: Home Assistant and Music Assistant over the loopback, the ESPHome nodes over the house lan.";

  # ── WHO ASKS FOR AN ACCOUNT ──
  #
  # `takers`, not `takersOn`: a secret is declared per SCOPE and has no host
  # to be asked about, where the user list below is per host and uses
  # `takersOn`. So every aspect in the flake declaring `takes.mqtt` gets a
  # credential minted in this broker's scope, and whether it also gets a login
  # depends on whether it is actually on this box.
  takers = den.lib.capability.takers "mqtt";
  takerNames = lib.attrNames takers;

  # The account a taker authenticates as: its own `username` where it states
  # one -- the ESPHome nodes are `sendspin`, a name already baked into device
  # configs -- and the aspect's own name otherwise.
  # `or` FIRES ON A MISSING ATTRIBUTE, NOT ON A NULL ONE. The contribution
  # exists for every taker and defaults to null, so `… or taker` never ran and
  # the account name came through as null -- "expected a string but found
  # null" from mosquitto's own user list.
  # `lib.defaultTo`, because `contributions` DOES carry every taker's default
  # and that default is null -- so the attribute exists and `or` would never
  # fire. That is exactly the bug this was written to fix (the account arrived
  # as `null`), and the helper says what the three-line `if` said.
  accountFor = contributions: taker: lib.defaultTo taker contributions.username.${taker};

in
{
  # ── MQTT, A NARROWING OF TCP ──
  #
  # Beside `http` and `grpc`: a consumer wanting a broker is answered by a
  # broker and never by a bare tcp giver on the same box.
  #
  # WHAT IT IS FOR, beyond resolution: the broker derives its USER LIST from
  # its takers, the way ../postgres.nix derives databases from
  # `takes.postgres`. Before this, ./mosquitto.nix
  # held a hand-written `users = { homeassistant = …; sendspin = …; }` and
  # two hashes a human had written into `/var/lib/mosquitto-passwords/` --
  # a login list that nothing could check against who actually connects, and
  # credentials nothing could rotate or recreate.
  #
  # A taker therefore says what topics it needs and gets an account; a
  # service that stops taking stops having one.
  den.capabilities.mqtt = {
    meta.description = "An MQTT broker. A narrowing of tcp.";
    side = "endpoint";
    includes = [ caps.tcp ];

    # The login is per (broker, client) and belongs to neither -- the same
    # reasoning `postgres` uses for a role password.
    takes = [ caps.credential ];

    taker.options = {
      # ── TOPICS, PER TAKER ──
      # `each`, not `union`: these become one `acl` block per user, and
      # merging two clients' topic lists into one would hand each the
      # other's. `readwrite #` is mosquitto's everything, which is what the
      # two existing logins already had.
      acl = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ "readwrite #" ];
      };

      # The account name, where it differs from the taking aspect's. The
      # ESPHome nodes authenticate as `sendspin`, which is the name the
      # add-on's own login list used and which is baked into device
      # configs -- so it is stated rather than renamed.
      username = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };
    };
  };

  # ── ITS OWN ASPECT, so it can GIVE what it listens on ──
  #
  # This file used to write into `den.aspects.homeassistant.nixos`, which left
  # the broker with nowhere to declare 1883: that aspect's `gives` lives in
  # ./core.nix, and a second definition there is a conflict rather than a
  # merge. Declared the way ./esphome.nix and ./music-assistant.nix already
  # are, and pulled in by the aspect that wants it.
  den.aspects.homeassistant.includes = [ den.aspects.mosquitto ];

  # ── WHAT HOME ASSISTANT NEEDS IN ORDER TO TALK TO THIS BROKER ──
  #
  # Contributed BY the broker through the edge, not listed in ./core.nix's
  # `extraComponents` -- which put "HA speaks MQTT" in a file that knows
  # nothing about mosquitto and left a dangling entry if the broker ever went
  # away. ./music-assistant.nix makes the same contribution for the same
  # reason, and its note records why a bare component list is the wrong
  # channel: it carries no wanter identity, so "who asked for this" is
  # unanswerable.
  den.aspects.mosquitto.takes.home-assistant.extraComponents = [ "mqtt" ];

  # ── A HOUSE BROKER, WHICH IS WHAT IT ALREADY WAS IN PRACTICE ──
  #
  # `reach` defaults to `host` (../../base/capability-types.nix: "the safe
  # answer is do not reach across machines"), and that default was never
  # questioned because every taker so far -- Home Assistant, Music Assistant,
  # the ESPHome dashboard -- is on this same box and resolves over the loopback.
  #
  # It was WRONG the whole time, and the ESPHome nodes are the proof: they
  # publish to this broker over the house lan, which is exactly why
  # `gives.tcp.ports.mqtt.expose` is `[ "lan-home" ]` a few lines down. A
  # listener deliberately opened to the lan, by a giver that will not resolve
  # for anything off the box, is two halves of one file disagreeing. Nothing
  # noticed because the nodes are not den hosts and so never asked the resolver.
  #
  # ../corsair-psu.nix is the first taker that is -- a publisher on auspc -- and
  # it got `dial "mqtt" {} auspc == null`, measured, rather than any message
  # about reach. `household` is the right width for the same reason
  # ./music-assistant.nix and ../openrgb.nix are: a shared appliance the house
  # uses, not infrastructure the whole fleet may reach.
  den.aspects.mosquitto.reach = "household";

  den.aspects.mosquitto.gives =
    { self, ... }:
    {
      # THE BROKER ITSELF, as the capability a client resolves rather than a
      # port number it is told. ../../base/capability-types.nix's `mqtt`
      # narrows `tcp`, so a consumer asking for a broker is answered by one
      # and never by some other tcp giver on the same box.
      # NO `hostname` HERE, and that is the honest answer rather than an
      # oversight. `hostname` is a CARRIER-side capability
      # (../../base/capability-types.nix: `side = "carrier"`) -- the loopback
      # furnishes `localhost`, tailscale furnishes its MagicDNS name, a zone
      # furnishes the published one -- and this broker has no name of its own
      # to claim. kanidm's `ldap` and ../mail.nix's `smtp` DO furnish one
      # because they serve TLS under a name they own; an MQTT listener on the
      # house lan is reached at an address.
      #
      # The consequence, worth knowing: `hostname` is a required `str`, so
      # FORCING a whole `mqtt` link throws "accessed but has no value defined".
      # Reading named fields off it -- which is what every consumer does -- is
      # unaffected. A composed link gets the name from its carrier, which is
      # where it belongs.
      mqtt = _: {
        name = self.name;
        port = mqttPort;
        ports.mqtt.meta.description = brokerPortDescription;
      };

      tcp = _: {
        name = self.name;
        ports.mqtt = {
          port = mqttPort;
          meta.description = brokerPortDescription;
          # THE HOUSE LAN. The ESPHome nodes publish to it, so loopback would
          # be useless -- and the tailnet is not where sensors live.
          expose = [ "lan-home" ];
        };
      };
    };

  # ── A REGISTERED GENERATOR, NOT A FUNCTION IN THIS FILE ──
  #
  # `mosquitto_passwd -b` writes `user:hash` and the module's
  # `hashedPasswordFile` wants the bare hash -- which is what the hand-written
  # runbook this replaces did with `cut -d: -f2-`.
  #
  # Declared where every other generator is (beside ../../network/vpn.nix's
  # `wireguard_pair` and ../mail.nix's `dkim_pair`) rather than as a `let`
  # here: a generator is a named, reusable thing that takes its parameters
  # from `secret.settings`, so anything else that ever needs a mosquitto hash
  # can ask for one by name.
  #
  # STDOUT IS THE SECRET (../../../CLAUDE.md), so mosquitto_passwd's own
  # chatter goes to stderr.
  den.aspects.agenix-rekey.age.generators.mosquitto_hash =
    {
      secret,
      pkgs,
      lib,
      decrypt,
      deps,
      ...
    }:
    ''
      tmp="$(mktemp)"
      trap 'rm -f "$tmp"' EXIT

      ${pkgs.mosquitto}/bin/mosquitto_passwd -c -b "$tmp" \
        ${lib.escapeShellArg secret.settings.username} \
        "$(${decrypt} ${lib.escapeShellArg (lib.head deps).file})" >&2

      cut -d: -f2- "$tmp"
    '';

  # ── ONE CREDENTIAL PER TAKER, DERIVED FROM WHO ASKS ──
  #
  # Was two bare hashes written by hand into `/var/lib/mosquitto-passwords/`
  # by a runbook in a comment, on the recorded reasoning that a generated
  # secret "has to be minted with the gpg-yubikey master identity before the
  # host will even evaluate".
  #
  # That does not hold: ../mail.nix declares its DKIM keys this way and
  # secondpc evaluates with none of them minted -- what needs the master
  # identity is `nix run .#gen-secrets`, a deploy step. What the hand-written
  # version did cost is a password nothing can rotate, nothing backs up and
  # nothing recreates on a rebuilt host.
  #
  # ONE SOURCE, TWO MATERIALISATIONS, the shape ../../security/user-pwd.nix
  # uses for `ivy-password` -> `ivy-pwd-hash`: the plaintext is what a client
  # authenticates WITH, the hash is what the broker checks against, and the
  # second is derived from the first so they cannot drift.
  den.aspects.mosquitto.secrets =
    { scoped, ... }:
    lib.listToAttrs (
      lib.concatMap (taker: [
        (lib.nameValuePair "${taker}-password" { generator.script = "random-base64-32"; })

        (lib.nameValuePair "${taker}-hash" {
          owner = "mosquitto";
          settings.username = taker;
          generator = {
            dependencies = [ scoped.secrets."${taker}-password" ];
            script = "mosquitto_hash";
          };
        })
      ]) takerNames
    );

  den.aspects.mosquitto.nixos =
    {
      scoped,
      host ? { },
      ...
    }:
    let
      # THE LOGINS, FROM WHO TAKES IT ANYWHERE THIS BROKER REACHES. A
      # hand-written `users` block could not be checked against who actually
      # connects; this cannot disagree, and a service that stops taking mqtt
      # stops having an account.
      #
      # ── `*Reaching`, NOT `*On`, AND THAT WAS A REAL BUG ──
      #
      # These two read `takersOn host` / `contributions host` -- takers whose
      # aspect is on THIS box -- which is right for a `reach = "host"` giver and
      # wrong the moment the broker serves the house. MEASURED on this file
      # before the change: the secrets for ../corsair-psu.nix were minted (the
      # `secrets` block above asks `den.lib.capability.takers`, which is fleet-wide and
      # has no host to filter on) and the broker's rendered user list was
      # `{ homeassistant; music-assistant; sendspin; }` with no `corsair-psu` in
      # it. A credential that exists, a password file deployed on the client, and
      # an account the broker has never heard of -- which fails as an
      # authentication error, pointing at the password rather than at the
      # account.
      #
      # It is the SAME gap ./core.nix describes for `contributionsReaching`: a
      # giver that reaches across hosts has to provision for the takers it
      # reaches, not for the ones beside it. Nothing noticed here either,
      # because every earlier taker was on this box.
      #
      # `takersReaching` yields `{ aspect; host; }` per entry rather than a bare
      # aspect, so only the NAMES are used below -- which is all the user list
      # ever wanted.
      asked = den.lib.capability.takersReaching den.aspects.mosquitto "mqtt" host;
      contributed = den.lib.capability.contributionsReaching den.aspects.mosquitto "mqtt" host;
    in
    {
      # NO `tmpfiles` DIRECTORY. It existed to hold two files a human wrote;
      # agenix places both, owned by mosquitto, at the modes it enforces.
      services.mosquitto = {
        enable = true;
        listeners = [
          {
            # ── RESOLVED FROM `expose`, NOT WRITTEN ──
            #
            # `gives.tcp.ports.mqtt.expose` is `[ "lan-home" ]` a few lines
            # above, and this was `0.0.0.0` written by hand beside it: the same
            # fact twice, in the two places that have to agree.
            #
            # It comes back as the wildcard, which is what the literal said --
            # and ../../base/capability.nix's own note explains why that is the
            # right answer rather than `192.168.0.26`: the ESPHome nodes reach
            # this over the house lan, and Home Assistant reaches it over the
            # loopback on this box, so the socket has to answer on both.
            address = den.lib.capability.bindAddressFor host den.aspects.mosquitto "tcp" "mqtt";
            port = mqttPort;

            # `acl` is not optional -- the module renders a user with an empty
            # acl as a `user <name>` stanza with no topics, which denies
            # everything. The capability's default is `readwrite #`, which is
            # what both existing logins already had.
            users = lib.listToAttrs (
              lib.map (
                taker:
                lib.nameValuePair (accountFor contributed taker) {
                  acl = contributed.acl.${taker} or [ "readwrite #" ];
                  hashedPasswordFile = scoped.mosquitto.secrets."${taker}-hash".path;
                }
              ) (lib.attrNames asked)
            );
          }
        ];
      };

      # NO FIREWALL LITERAL. The port is furnished above and
      # ../../base/firewall.nix opens it on exactly the carriers named there
      # -- the house lan, and nothing else. A literal opens it on every wire.
    };
}
