# Tailscale, as a peer to ./vpn.nix rather than a replacement for it.
#
# The wireguard tunnel in ./vpn.nix is a hub-and-spoke through `secondpc` at a
# fixed endpoint: every client dials that one box, so when it is down or its
# address moves, nothing reaches anything. Tailscale is here for the cases where
# that is not good enough -- reaching the laptop from a phone on cellular, or
# from behind a NAT that neither end controls -- because it negotiates directly
# and falls back to a DERP relay when it cannot.
#
# Credentials are an OAuth client, not a pre-auth key, which is why no machine
# that can mint at boot stores a key of its own. An OAuth client does not expire
# the way an auth key does (90 days at the outside), and keys minted from it are
# made on demand and thrown away -- so a phone is enrolled by minting one at that
# moment rather than by decrypting something that has been sitting in the repo
# going stale.
#
# The exception is a host that cannot reach the API at boot: `authKeySecret`
# mints it one key at `gen-secrets` time instead, which does go stale on the
# same 90-day clock -- re-running `gen-secrets` after deleting the generated
# file is what refreshes it.
#
# BOOTSTRAP:
#
#   1. Make an OAuth client at
#      https://login.tailscale.com/admin/settings/oauth
#      with the `auth_keys` write scope, granted over ${nodeTag}.
#   2. Put this in the tailnet ACL, or step 3 is rejected:
#        "tagOwners": { "${nodeTag}": ["autogroup:admin"] }
#   3. nix run .#secret-edit -- secrets/static/tailscale_auth.age
#      Two lines, no quotes, no `export`:
#        CLIENT_ID=...
#        CLIENT_SECRET=tskey-client-...
#   4. Rebuild. `tailscale-autoconnect` enrols this machine at boot.
#
# Enrol a phone or anything else this flake does not build:
#
#   nix run .#tailscale-client-key            # asks which device
#   nix run .#tailscale-client-key -- mobile  # or name it
#
# which mints a single-use, preauthorized key from the OAuth client and prints
# it, to paste into the Tailscale app's "Custom auth key" field. If the stored
# client cannot be decrypted (no Yubikey in this checkout), it asks for a
# CLIENT_ID/CLIENT_SECRET instead, so step 3 above is a convenience rather than
# a prerequisite.
#
# Once the phone is on the tailnet, t3code is reachable from it over the
# tailnet's own HTTPS -- see `nix run .#t3code-pair` in
# ../dev/llms/t3code.nix for the pairing link.
{
  den,
  lib,
  fleet,
  carrierAttachment,
  routeLib,
  ...
}:
let
  # OAuth-minted nodes MUST be tagged -- an OAuth client secret cannot enrol an
  # untagged node, and a tag it does not own is refused. One tag for everything
  # this flake builds; devices enrolled by hand stay untagged and keep their own
  # ACL treatment.
  nodeTag = "tag:nix";

  # ── THE INTERFACE NAME, DECLARED ONCE AND SET FROM HERE ──
  #
  # The capability layer is the source and `services.tailscale.interfaceName`
  # below is set FROM it, not the other way round. Reading it back off the
  # host's config would mean evaluating that host to answer "which wire does a
  # tailnet-exposed port open on" -- several GB for one string -- which is the
  # whole reason an attachment is entity-level data (../hosts/fleet.nix).
  #
  # `tailscale0` is also nixpkgs' default, so this changes nothing today; what
  # it buys is that changing it changes both halves at once.
  interfaceName = "tailscale0";

  # ── AND IT IS A PER-HOST FACT AFTER ALL ──
  #
  # `tailscale0` is what tailscaled creates WHEN IT HAS A TUN. A container with
  # no TUN and no NET_ADMIN runs `--tun=userspace-networking` and has no
  # tailscale interface at all -- ../hosts/celler2.nix -- so furnishing
  # `tailscale0` there tells a consumer to use a wire that does not exist, and
  # ../base/firewall.nix would open that host's tailnet ports on it.
  #
  # Read off the host ENTITY and not its config, for the reason stated above:
  # an attachment is entity-level data precisely so this question costs no host
  # eval. `attachments.<carrier>.interface` is the slot ../hosts/fleet.nix
  # already declares and checks for the lan, so there is nothing new to
  # declare -- and a host that states nothing keeps `tailscale0`.
  # `lib.defaultTo`, NOT `or`: `interface` is a declared option with a null
  # default, so for a host that states the attachment and omits the interface
  # the attribute EXISTS and `or` never fires -- the null flows on and this
  # carrier furnishes no wire. ../../CLAUDE.md's first trap; measured here
  # before it had a caller that could hit it.
  ifaceFor = h: lib.defaultTo interfaceName (h.attachments.tailscale.interface or null);

  # Devices that are part of the tailnet but that this flake does not build, so
  # there is nothing to configure for them -- only a key to hand over. Declared
  # rather than free-form so `.#tailscale-client-key` can offer a list and so a
  # typo mints a key described as "mobil" instead of failing.
  #
  # Read out of the fleet (../hosts/fleet.nix): a device is a den host with a
  # `device` record, declared next to the machines it sits alongside.
  #
  # Nothing is stored per client: the key is minted at the moment you enrol the
  # device and is single-use, so the list costs nothing to extend -- give a
  # host a `device` record and it appears here.
  clients = lib.mapAttrs (_: meta: meta.device.description) (
    lib.filterAttrs (_: meta: (meta.device or null) != null) fleet
  );
  clientNames = lib.attrNames clients;

  # Shared by the autoconnect daemon and the key-minting app. Tailscale's OAuth
  # is plain client_credentials, so this is a token exchange followed by one
  # authenticated POST -- no SDK, and nothing that has to be kept in step with a
  # library version.
  #
  # Contract with the caller, kept deliberately small because this is inlined
  # into two different scripts: set `ts_env_file` to the KEY=VALUE file and
  # `ts_description` to whatever should show against the key in the admin
  # console. Sets `authkey` on success and exits non-zero on every failure.
  #
  # Both are shell variables rather than Nix-interpolated strings so the app can
  # point this at a temp file it made at runtime, and so the description can
  # carry the real hostname instead of whatever the module system thinks it is
  # at eval time.
  #
  # `reusable` is the one axis that differs between the two ways this is used.
  # A daemon that mints at every boot spends its key immediately, so a
  # single-use one is strictly better; a key minted ONCE by `gen-secrets` and
  # stored for a host (see `authKeySecret`) is replayed on every restart, so a
  # single-use one would authenticate that host exactly once.
  mkMintKeyScript =
    {
      reusable ? false,
    }:
    ''
      if [ ! -r "$ts_env_file" ]; then
        echo "tailscale: $ts_env_file is not readable" >&2
        exit 1
      fi

      # Sourced in a subshell-free way but scrubbed immediately after: CLIENT_ID
      # and CLIENT_SECRET are the long-lived credential and have no business
      # staying in the environment of anything this later execs.
      # The source has to sit on its own line with the directive immediately
      # above it. As `set -a; . "$f"; set +a` on one line, shellcheck attaches the
      # disable to `set -a` and still fails the build on SC1090 -- which it does
      # at *build* time, so it passes eval and only bites on switch.
      set -a
      # shellcheck disable=SC1090
      . "$ts_env_file"
      set +a

      if [ -z "''${CLIENT_ID:-}" ] || [ -z "''${CLIENT_SECRET:-}" ]; then
        echo "tailscale: $ts_env_file must set CLIENT_ID and CLIENT_SECRET" >&2
        exit 1
      fi

      # NOT `curl -f`. writeShellApplication sets `errexit`, so a `-f` curl inside
      # a command substitution kills the script AT THE ASSIGNMENT -- which made
      # every diagnostic below this point dead code and turned a real API error
      # into 199 log lines reading only "curl: (22)". Capture the status and the
      # body instead, and say what the API actually objected to.
      token_body="$(mktemp)"
      token_code="$(curl -sS -o "$token_body" -w '%{http_code}' \
        -d "client_id=$CLIENT_ID" \
        -d "client_secret=$CLIENT_SECRET" \
        https://api.tailscale.com/api/v2/oauth/token || echo 000)"

      if [ "$token_code" != "200" ]; then
        echo "tailscale: OAuth token exchange failed (HTTP $token_code)" >&2
        echo "  response: $(head -c 500 "$token_body")" >&2
        echo "  check CLIENT_ID/CLIENT_SECRET in $ts_env_file" >&2
        rm -f "$token_body"
        exit 1
      fi

      access_token="$(jq -r '.access_token // empty' <"$token_body")"
      rm -f "$token_body"

      if [ -z "$access_token" ]; then
        echo "tailscale: token endpoint returned 200 but no access_token" >&2
        exit 1
      fi

      # The key description is validated control-side against a set narrower than
      # JSON's -- parentheses are rejected -- and all you get back is HTTP 400
      # "description had invalid characters", which reads like an auth problem and
      # is not one. Fold anything outside the safe set to a hyphen and cap the
      # length, so a hostname can never wedge the daemon into a retry loop.
      ts_description="$(printf '%s' "$ts_description" | tr -c 'A-Za-z0-9 ._-' '-' | cut -c1-50)"

      # Built with jq rather than a Nix-side toJSON so `ts_description` can be a
      # runtime value, and so the description is escaped by something that
      # actually knows JSON.
      payload="$(jq -n \
        --arg description "$ts_description" \
        --arg tag ${lib.escapeShellArg nodeTag} \
        '{description: $description,
          capabilities: {devices: {create: {
            reusable: ${lib.boolToString reusable},
            ephemeral: false,
            preauthorized: true,
            tags: [$tag]}}}}')"

      # `-` means "the tailnet these credentials belong to", so this never has to
      # name the tailnet.
      key_body="$(mktemp)"
      key_code="$(curl -sS -o "$key_body" -w '%{http_code}' \
        -H "Authorization: Bearer $access_token" \
        -H "Content-Type: application/json" \
        -d "$payload" \
        https://api.tailscale.com/api/v2/tailnet/-/keys || echo 000)"

      unset CLIENT_ID CLIENT_SECRET access_token

      if [ "$key_code" != "200" ]; then
        echo "tailscale: minting an auth key failed (HTTP $key_code)" >&2
        echo "  response: $(head -c 500 "$key_body")" >&2
        echo "  request:  $payload" >&2
        echo "  check ${nodeTag} is in the ACL's tagOwners and the client has auth_keys write" >&2
        rm -f "$key_body"
        exit 1
      fi

      authkey="$(jq -r '.key // empty' <"$key_body")"
      rm -f "$key_body"

      if [ -z "$authkey" ]; then
        echo "tailscale: key endpoint returned 200 but no key field" >&2
        exit 1
      fi
    '';

  mintKeyScript = mkMintKeyScript { };

  # ── THE TAILNET'S ROUTES, DERIVED (../network/routes.nix) ──
  # What this host advertises and which exit node it uses follow from the
  # route takes across the fleet; `tailscale set` applies them without
  # touching the login, and an empty plan clears what an earlier one set.
  routesScript =
    pkgs: tailscale: plan:
    pkgs.writeShellScript "tailscale-routes" ''
      for _ in $(seq 1 60); do
        tailscale status --peers=false >/dev/null 2>&1 && break
        sleep 2
      done
      ${tailscale}/bin/tailscale set \
        --advertise-routes=${lib.concatStringsSep "," plan.advertiseRoutes} \
        --advertise-exit-node=${lib.boolToString plan.advertiseExit} \
        --exit-node=${lib.defaultTo "" plan.exitNode} \
        --exit-node-allow-lan-access=true
    '';

  # One name per machine: the tailnet hostname, the admin-console nickname and
  # the auth key's description are all the machine's own hostname, so nothing
  # downstream has to learn a second name for the same box. Shared by both
  # backends so the flag the daemon runs with and the flag
  # `services.tailscale.extraUpFlags` publishes cannot drift apart.
  upFlagsFor = config: rec {
    hostName = config.networking.hostName;
    upFlags = [ "--hostname=${hostName}" ];
  };

  # An authkey for one host, minted ONCE by `nix run .#gen-secrets` from the
  # shared OAuth client and kept master-encrypted at
  # secrets/generated/<host>/tailscale_authkey.age.
  #
  # For a host that cannot run the boot-time minting daemon above: the nix-oci
  # container image (../hosts/celler2.nix) starts tailscaled by hand from
  # cellerd's preStart, and baking curl/jq into the image to re-mint on every
  # restart buys nothing when the admin host can mint it once instead. The
  # OAuth client never reaches the target -- only the key it produced does.
  # A PER-ENTRY module function, which is what lets the dependency be found
  # rather than passed: `age.secrets.<n>` is a submodule, so lib/age-scoped.nix
  # binds `secrets` to the enclosing scope keyed by short name
  # (`entryType`'s `_module.args`). So the generator picks the scope's own
  # `auth` entry up itself, and that entry is a real `age.secrets` value --
  # which it has to be, because apps/generate.nix reads `dep.id`,
  # `dep.generator` and `dep.rekeyFile` off a dependency and a literal
  # `{ rekeyFile = ...; }` fails with `attribute 'generator' missing`.
  #
  # The contract is just the name: whatever scope declares this must also
  # declare the OAuth client as `auth`, the way `den.aspects.tailscale` does.
  # ONE script for every stored authkey, registered as `age.generators` and
  # named by `generator.script = "tailscale-authkey"`. Its per-secret
  # parameters come off `secret.settings`, upstream's channel for exactly
  # this, so nothing here closes over a call-site argument.
  authKeyScript =
    {
      secret,
      pkgs,
      decrypt,
      deps,
      ...
    }:
    ''
      export PATH=${
        lib.makeBinPath [
          pkgs.coreutils
          pkgs.curl
          pkgs.jq
        ]
      }:$PATH

      ts_env_file="$(mktemp)"
      trap 'rm -f "$ts_env_file"' EXIT
      ${decrypt} ${lib.escapeShellArg deps.oauth.file} > "$ts_env_file"

      # The device shows on the tailnet under its own hostname, so the key is
      # described by the same name rather than a decorated variant.
      ts_description=${lib.escapeShellArg secret.settings.device}
      ${mkMintKeyScript { reusable = true; }}

      printf '%s\n' "$authkey"
    '';

  # What a call site writes. No `rekeyFile`: the default puts it under the
  # host's `generatedSecretsDir` with the eval-time half of `derivedFrom`
  # folded into the name.
  #
  # `settings` is both the generator's parameters and what the key is a
  # function of, so `derivedFrom` is just `settings`. `nodeTag` is in there
  # because the key is minted FOR that tag and a node cannot advertise one its
  # key does not carry -- so a changed tag has to mint a new key, and being in
  # the filename is what makes `agenix generate` do it, leaving the old key
  # intact until it is pruned.
  authKeySecret =
    device:
    # Still a per-entry module function, for `secrets.auth`: a generator
    # dependency has to be a real `age.secrets` entry, and this is where the
    # enclosing scope's entries are bound.
    { secrets, ... }:
    let
      settings = {
        inherit device nodeTag;
      };
    in
    {
      inherit settings;
      generator = {
        tags = [ "tailscale_authkey" ];
        script = "tailscale-authkey";
        dependencies.oauth = secrets.auth;
        derivedFrom = settings;
      };
    };
in
{
  # The shared generator, registered once for the whole fleet rather than on
  # `den.aspects.tailscale`: ../hosts/celler2.nix stores an authkey without
  # including that aspect (it has no boot-time daemon to run), and a named
  # generator has to be registered wherever a secret names it. It costs
  # nothing to carry -- an unapplied function that is never run unless some
  # secret's `generator.script` asks for it.
  den.default.age.generators.tailscale-authkey = authKeyScript;

  den.aspects.tailscale = {
    # ── WHAT THIS ASPECT GIVES, for ../base/capability.nix ──
    #
    # Including this aspect IS being on the tailnet. That is the whole point of
    # membership-by-inclusion: there is no `enrolled = true` to write and
    # nothing that can disagree with reality, which is what §2.7 asked for and
    # could not have while the question was asked from outside den.
    #
    # `prefix` is what makes this a SEGMENT rather than a per-host thing like a
    # loopback -- several hosts attach to it and are thereby connected. CGNAT,
    # fixed by tailscale rather than chosen here.
    #
    # `addressOf` answers where a peer is reachable. NOT a `100.x` address:
    # those are assigned by the coordination server and are not knowable at eval
    # time, which is the fact that blocked declaring tailnet attachments at all.
    # A NAME is knowable -- MagicDNS gives every node
    # `<host>.tailef3db9.ts.net` -- and only its resolution is deferred, which
    # is §2.7a's "eval time knows the SHAPE, run time knows what is up".
    # GIVES TAKES SELF, which is the thing an empty `gives.tcp = { }` could not
    # do. A capability is not a token saying "I have tcp" -- it is what the
    # consumer RECEIVES, and only the giver can furnish it, because only the
    # giver knows it addresses its members by MagicDNS name rather than by a
    # `100.x` the coordination server assigns at run time.
    #
    # So the fiber for "how do I reach this peer over the tailnet" lives HERE,
    # in the segment that knows the answer, rather than in a consumer that would
    # otherwise have to name `tailef3db9.ts.net` itself. A consumer writes
    # `uses.tcp` and reads `tcp.address`; it never learns which carrier won, and
    # the same line is right over the lan.
    gives =
      { self, ... }:
      let
        # DERIVED FROM THE PEER, never written out. The four MagicDNS names were
        # literals in a deleted ../network/tailnet.nix, whose own comment argued
        # that writing them was "cheaper than a mechanism that exists for one
        # subtype" -- true of the registry it lived in, and false here: the
        # mechanism is this one line, and it cannot go stale when a host is
        # added or renamed.
        nameOf = peer: "${lib.toLower peer.name}.${self.domain}";
      in
      {
        # A NAME, GIVEN AS ITS OWN CAPABILITY. This carrier has a namespace and
        # the lan does not, which is why the two are separable: a consumer
        # needing a name a certificate or a known_hosts entry can match asks for
        # `hostname` and is answered only by a carrier that actually serves one.
        hostname = { peer, ... }: {
          hostname = nameOf peer;
          # MagicDNS answers the bare name inside the tailnet too, which is what
          # `tailscale ssh secondpc` and ../hosts/iphone.nix's `MagicDNSSuffix`
          # lookup both rely on. The carrier furnishes it so no consumer has to
          # know that is true.
          aliases = [ (lib.toLower peer.name) ];
          inherit (self) domain;
        };

        # THE NAME IS ALSO THE ADDRESS HERE, and that is a property of this
        # carrier rather than a shortcut: a `100.x` is assigned by the
        # coordination server and is not knowable at eval time, which is the
        # fact that blocked declaring tailnet attachments at all. A name is
        # knowable and only its RESOLUTION is deferred -- §2.7a's "eval time
        # knows the shape, run time knows what is up".
        # 1280, which is LOWER than ./vpn.nix's 1420 -- so a flow needing 1500
        # is excluded from BOTH tunnels, and ../services/vban.nix is right for a
        # stronger reason than its comment states. Verify against a live tunnel
        # before relying on the exact number.
        #
        # No body cap: this is a WireGuard tunnel, not a proxy.
        # THE INTERFACE IS DERIVED, where the lan's is declared, and the
        # difference is the point: tailscaled always creates `tailscale0`, so
        # INCLUDING this aspect is the whole attachment -- there is no
        # per-host fact to state and nothing that could disagree with the
        # running box. ../hosts/fleet.nix's `attachments.lan-home` exists
        # because a bridge name is not derivable in that way.
        #
        # What it buys: ../base/firewall.nix can open a port on `tailscale0`
        # alone, so a service reachable from the tailnet and not from the LAN
        # is finally a thing this tree can say.
        #
        # `mtu` and `maxBodyBytes` are READ OFF THE ASPECT: both are the same
        # for every pair in the tunnel, so they are carrier facts rather than
        # halves of a relation -- ./carrier-attachment.nix's roster reads the
        # one declaration instead of restating them.
        tcp =
          { peer, host, ... }:
          {
            address = nameOf peer;
            interface = ifaceFor host;
            inherit (self) mtu maxBodyBytes;
            # No `carrier`: derived from this aspect (../base/capability.nix).
          };
        udp =
          { peer, host, ... }:
          {
            address = nameOf peer;
            interface = ifaceFor host;
            inherit (self) mtu maxBodyBytes;
            # No `carrier`: derived from this aspect (../base/capability.nix).
          };

        segment = _: {
          inherit (self) prefix spans;
        };

        # Its route to its own segment, in the table tailscaled installs and
        # owns (52). No `mark`: tailscaled steers into it itself, so
        # ../network/routes.nix renders nothing for it -- it is here so the
        # route graph has the tailnet in it.
        route =
          { host, ... }:
          {
            segment = self.name;
            inherit (self) prefix;
            table = 52;
            dev = ifaceFor host;
          };
      };

    # A TUNNEL, so below the wire: tailscale is 1280 MTU and rides the internet,
    # and a flow that cannot fit that has no business resolving here.
    preference = 40;

    # ONE tunnel, shared -- the same kind of thing as the lan and not the same
    # kind of thing as a loopback. See `spans` in ../base/capability-types.nix.
    spans = "many-hosts";

    probe = { link, ... }: "ping -c1 -W2 ${link.address}";

    prefix = "100.64.0.0/10";
    domain = "tailef3db9.ts.net";

    # 1280, which is LOWER than ./vpn.nix's 1420 -- so a flow needing 1500 is
    # excluded from BOTH tunnels. No body cap: this is a WireGuard tunnel, not
    # a proxy. On the aspect rather than inside the link for
    # ./carrier-attachment.nix's reason. Verify against a live tunnel before
    # relying on the exact number.
    mtu = 1280;
    maxBodyBytes = null;
    # The tailnet's own identifier, recorded for the operator; nothing derives
    # from it.
    tailnetId = "TKgCTKkv8j11CNTRL";

    includes = [ den.aspects.agenix-rekey ];

    # The escape hatch lib/age-scoped.nix documents for exactly this shape of
    # wedge, and it is not optional here.
    #
    # A scope's `service` defaults to the aspect name, and the darwin backend
    # infers `restartUnits` from it with `config.launchd.daemons ? tailscale`.
    # That membership test forces the ATTRIBUTE NAMES of `launchd.daemons`,
    # which forces every definition of it -- including agenix's own, whose
    # entire config block is `mkIf (cfg.secrets != { } || ...)`. Evaluating that
    # condition forces `age.secrets`, which is what the backend was computing:
    # age.secrets -> launchd.daemons -> age.secrets, and the host dies with
    # `infinite recursion` from inside agenix, pointing nowhere near here.
    #
    # It bites this aspect and not ./vpn.nix because a scope only reaches for
    # launchd at all on darwin, and `vpn-secrets` is not a name any of this
    # resolves against. Nothing is lost by switching inference off: the only
    # consumer is the `tailscale-autoconnect` unit below, which reads the
    # credentials at boot rather than needing a reload when they change. The
    # setting is aspect-wide, so the nixos branch inherits it -- harmlessly,
    # for the same reason.
    secretSettings.service = null;

    secrets =
      { age, ... }:
      {
        # Hand-written rather than generated: an OAuth client is minted in the
        # admin console and has no offline derivation, so there is nothing for
        # a generator to compute.
        #
        # `intermediary`, so it never reaches a target. It is decrypted on the
        # ADMIN host at `gen-secrets` time to mint the per-host key below, and
        # that key is all a machine ever holds. The client can mint for the
        # whole tailnet, so keeping it off every node is the point -- the same
        # reasoning ./cloudflare.nix applies to its account token, and what
        # ../hosts/celler2.nix already did for the one host that could not mint
        # at boot.
        auth = {
          rekeyFile = ../../secrets/static/tailscale_auth.age;
          intermediary = true;
        };

        # This host's own key, minted once at generate time rather than on
        # every boot. `generatedSecretsDir` is `secrets/generated/<host>/`, so
        # its basename is the hostname -- the same way ./cloudflare.nix names
        # the tunnel it mints for.
        ts_authkey = authKeySecret (baseNameOf age.rekey.generatedSecretsDir);
      };

    # NixOS does have `services.tailscale.authKeyFile`, which is why darwin
    # needs a launchd daemon and this does not need an equivalent -- but it
    # still cannot point at the stored secret, because what is stored is an
    # OAuth *client* (CLIENT_ID/CLIENT_SECRET), not a key. So the same shared
    # minting script runs once at boot, guarded by the backend state so an
    # already-enrolled node never mints a key it will not spend.
    nixos =
      {
        config,
        lib,
        pkgs,
        scoped,
        host ? { },
        ...
      }:
      let
        authKeyFile = scoped.tailscale.secrets.ts_authkey.path;
        inherit (upFlagsFor config) upFlags hostName;
        plan = routeLib.tailnetPlan host;
        forwarding =
          {
            true = "both";
            false = "client";
          }
          .${lib.boolToString (plan.advertiseRoutes != [ ] || plan.advertiseExit)};
      in
      {
        services.tailscale = {
          enable = true;
          openFirewall = true;
          # SET FROM THE CARRIER, see `interfaceName` above: the link this
          # aspect furnishes says traffic arrives on it, and this is what
          # makes that true rather than a hope.
          #
          # `mkDefault` because a host CAN know better and one does:
          # ../hosts/celler2.nix has no TUN and runs userspace networking. A
          # plain definition here collided with that one outright, which is how
          # the per-host case was found.
          interfaceName = lib.mkDefault interfaceName;
          # `--accept-routes` below only does anything with this on.
          # A host that advertises a route or an exit node forwards for peers.
          useRoutingFeatures = lib.mkDefault forwarding;
          # Inert on its own here -- nixpkgs only passes `extraUpFlags` to its
          # own autoconnect unit, which needs an `authKeyFile` this host does
          # not have. It is declared because it is also the published record of
          # how this node joins: ../base/celler/server.nix reads the
          # `--hostname=` back out of it to build the tailscale address it hands
          # consumers, and the daemon below is passed the same list.
          extraUpFlags = upFlags;
        };

        systemd.services.tailscale-routes = {
          description = "Apply this host's derived tailnet routes";
          after = [ "tailscale-autoconnect.service" ];
          wants = [ "tailscale-autoconnect.service" ];
          wantedBy = [ "multi-user.target" ];
          path = [
            pkgs.coreutils
            config.services.tailscale.package
          ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            ExecStart = routesScript pkgs config.services.tailscale.package plan;
          };
        };

        systemd.services.tailscale-autoconnect = {
          after = [
            "tailscaled.service"
            "network-online.target"
          ];
          wants = [
            "tailscaled.service"
            "network-online.target"
          ];
          wantedBy = [ "multi-user.target" ];
          path = [
            config.services.tailscale.package
            pkgs.coreutils
            pkgs.jq
          ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            # The key is stored and reusable, so a retry spends nothing -- but
            # a persistent failure (an ACL missing ${nodeTag} being the likely
            # one, or an expired key) still should not retry tightly.
            Restart = "on-failure";
            RestartSec = 60;
          };
          script = ''
            set -eu

            for _ in $(seq 1 60); do
              if tailscale status --json >/dev/null 2>&1; then break; fi
              sleep 1
            done

            state="$(tailscale status --json 2>/dev/null | jq -r '.BackendState // "NoState"' || echo NoState)"
            if [ "$state" != "NeedsLogin" ] && [ "$state" != "NoState" ]; then
              echo "tailscale: backend is ''${state:-unknown}, nothing to do"
              exit 0
            fi

            # `file:` rather than the key itself: tailscale reads the path,
            # so the credential never appears in the process table.
            tailscale up \
              --auth-key file:${lib.escapeShellArg authKeyFile} \
              --advertise-tags ${lib.escapeShellArg nodeTag} \
              --accept-routes \
              ${lib.escapeShellArgs upFlags}
            echo "tailscale: up as ${hostName} (${nodeTag})"
          '';
        };
      };

    darwin =
      {
        config,
        pkgs,
        scoped,
        host ? { },
        ...
      }:
      let
        authKeyFile = scoped.tailscale.secrets.ts_authkey.path;
        inherit (upFlagsFor config) upFlags hostName;
        plan = routeLib.tailnetPlan host;
        forwarding =
          {
            true = "both";
            false = "client";
          }
          .${lib.boolToString (plan.advertiseRoutes != [ ] || plan.advertiseExit)};
      in
      {
        services.tailscale.enable = true;

        # nix-darwin's module only runs `tailscaled`; it has no equivalent of
        # NixOS's `services.tailscale.authKeyFile`, so enrolling the node is
        # this daemon's job.
        #
        # It is not `RunAtLoad`-and-exit-once by accident: tailscaled takes a
        # moment to create its socket, and a node that has been logged out (or
        # whose key expired) needs enrolling again without a rebuild. So it
        # waits for the socket, acts only when the backend actually says
        # NeedsLogin, and is a no-op on every other boot.
        launchd.daemons.tailscale-routes = {
          command = "${routesScript pkgs config.services.tailscale.package plan}";
          path = [
            pkgs.coreutils
            config.services.tailscale.package
          ];
          serviceConfig = {
            Label = "com.ivy.tailscale-routes";
            RunAtLoad = true;
            StandardErrorPath = "/var/log/tailscale-routes.log";
            StandardOutPath = "/var/log/tailscale-routes.log";
          };
        };

        launchd.daemons.tailscale-autoconnect = {
          command = "${
            pkgs.writeShellApplication {
              name = "tailscale-autoconnect";
              # writeShellApplication prepends these to PATH, so naming every
              # external command here is what stops the daemon depending on
              # whatever launchd happens to hand it.
              runtimeInputs = [
                config.services.tailscale.package
                pkgs.coreutils
                pkgs.jq
              ];
              text = ''
                for _ in $(seq 1 60); do
                  if tailscale status --json >/dev/null 2>&1; then break; fi
                  sleep 1
                done

                state="$(tailscale status --json 2>/dev/null | jq -r '.BackendState // "NoState"' || echo NoState)"
                if [ "$state" != "NeedsLogin" ] && [ "$state" != "NoState" ]; then
                  echo "tailscale: backend is ''${state:-unknown}, nothing to do"
                  exit 0
                fi

                # `file:` rather than the key itself, so the credential never
                # appears in the process table.
                #
                # Not `exec`, because what happens on failure matters. launchd
                # restarts this after ~10s (KeepAlive.SuccessfulExit), so a
                # persistent failure -- an ACL missing ${nodeTag} being the
                # likely one, or a key that has expired since it was minted --
                # would otherwise retry six times a minute until noticed.
                # Backing off to a minute bounds that, and says plainly what to
                # go and look at.
                if tailscale up \
                  --auth-key file:${lib.escapeShellArg authKeyFile} \
                  --advertise-tags ${lib.escapeShellArg nodeTag} \
                  --accept-routes \
                  ${lib.escapeShellArgs upFlags}
                then
                  echo "tailscale: up as ${hostName} (${nodeTag})"
                  exit 0
                fi

                echo "tailscale: 'tailscale up' failed -- check that ${nodeTag} is in the tailnet ACL's tagOwners, that the OAuth client owns it, and that ${hostName}'s stored key has not expired (re-mint with: nix run .#gen-secrets)" >&2
                sleep 60
                exit 1
              '';
            }
          }/bin/tailscale-autoconnect";
          serviceConfig = {
            Label = "com.ivy.tailscale-autoconnect";
            RunAtLoad = true;
            KeepAlive.SuccessfulExit = false;
            StandardErrorPath = "/var/log/tailscale-autoconnect.log";
            StandardOutPath = "/var/log/tailscale-autoconnect.log";
          };
        };
      };
  };

  # For hosts that authenticate from a stored key rather than the boot-time
  # daemon above. `nodeTag` comes with it because `tailscale up` has to
  # advertise the same tag the key was minted for.
  den.lib.tailscale = {
    inherit authKeySecret nodeTag;
  };

  # Mint a key for a device this flake does not build -- a phone, a tablet, a
  # borrowed machine. Single-use and preauthorized, so it is spent the moment
  # the device enrols and is worth nothing afterwards if it leaks.
  #
  # Reads the OAuth client through the *master* identity rather than the
  # laptop's deployed copy, so this works from any checkout the Yubikey is
  # plugged into, not only from the machine the secret was rekeyed onto.
  perSystem =
    {
      pkgs,
      config,
      ...
    }:
    {
      # A package as well as an app, deliberately. `apps.<x>.program` is a
      # string, so there is no way to `nix build` it -- and writeShellApplication
      # runs shellcheck in its BUILDER, which means an app-only script is never
      # linted until something realises it. That is how an SC1090 failure in
      # here survived a green `nix eval` of the whole host: eval does not build.
      # `nix build .#tailscale-client-key` now exercises it.
      packages.tailscale-client-key = pkgs.writeShellApplication {
        name = "tailscale-client-key";
        runtimeInputs = [
          config.packages.secret-view
          pkgs.coreutils
          pkgs.curl
          pkgs.jq
        ];
        text = ''
          known=(${lib.concatStringsSep " " (map lib.escapeShellArg clientNames)})

          name="''${1:-}"

          # No argument: ask, rather than print a usage line and quit. This
          # is a command you run once per device and rarely enough to have
          # forgotten the names.
          if [ -z "$name" ]; then
            # Opening it, not `[ -r /dev/tty ]`: the device node tests
            # readable even with no controlling terminal attached, so that
            # check passes and the `read` then dies with "Device not
            # configured". Actually opening it is the only honest test.
            if ! { : </dev/tty; } 2>/dev/null; then
              echo "usage: nix run .#tailscale-client-key -- <${lib.concatStringsSep "|" clientNames}>" >&2
              exit 2
            fi
            echo "Which device is this key for?" >&2
            ${lib.concatStringsSep "\n" (
              lib.mapAttrsToList (n: desc: ''echo "  ${n} -- ${desc}" >&2'') clients
            )}
            printf 'device: ' >&2
            IFS= read -r name </dev/tty
          fi

          matched=0
          for k in "''${known[@]}"; do
            [ "$k" = "$name" ] && matched=1
          done
          if [ "$matched" -ne 1 ]; then
            echo "unknown device '$name' -- known: ''${known[*]}" >&2
            # The list is derived from the fleet, so that is where a new device
            # goes; there is no list in this file to edit.
            echo "(give its den host a \`device\` record, e.g. aspects/hosts/iphone.nix)" >&2
            exit 2
          fi

          # Decrypt to a private temp file rather than a variable: the
          # shared minting code sources a KEY=VALUE file, and a
          # process-substitution FD would not survive `set -a; . <file>`
          # identically across shells.
          tmp="$(mktemp -d)"
          trap 'rm -rf "$tmp"' EXIT

          src="secrets/static/tailscale_auth.age"
          if [ -e "$src" ] && (umask 077; secret-view "$src" > "$tmp/env" 2>/dev/null); then
            :
          else
            # Fall back to asking. The stored client needs the Yubikey to
            # decrypt, so this is what makes the command usable from a
            # checkout that does not have it -- and what lets you mint a key
            # with a *different* OAuth client without editing the secret.
            if ! { : </dev/tty; } 2>/dev/null; then
              echo "Could not decrypt $src, and there is no terminal to ask on." >&2
              echo "Run it from your own shell, where gpg can prompt for the Yubikey:" >&2
              echo "  nix run .#tailscale-client-key -- mobile" >&2
              exit 1
            fi
            echo "Could not decrypt $src -- falling back to asking." >&2
            echo "From https://login.tailscale.com/admin/settings/oauth (needs auth_keys write over ${nodeTag}):" >&2
            printf 'CLIENT_ID: ' >&2
            IFS= read -r oauth_id </dev/tty
            printf 'CLIENT_SECRET: ' >&2
            # -s: the secret is not echoed into a shared terminal's scrollback.
            IFS= read -rs oauth_secret </dev/tty
            printf '\n' >&2
            (
              umask 077
              printf 'CLIENT_ID=%s\nCLIENT_SECRET=%s\n' "$oauth_id" "$oauth_secret" > "$tmp/env"
            )
            unset oauth_id oauth_secret
          fi

          ts_env_file="$tmp/env"
          ts_description="$name"
          ${mintKeyScript}

          echo "" >&2
          echo "Single-use ${nodeTag} key for '$name' -- paste into the Tailscale app's" >&2
          echo "\"Custom auth key\" field. It is spent once the device enrols." >&2
          echo "" >&2
          printf '%s\n' "$authkey"
        '';
      };

      apps.tailscale-client-key = {
        type = "app";
        program = lib.getExe config.packages.tailscale-client-key;
      };
    };

  # ── THE TAILNET'S HALF OF AN ATTACHMENT ──
  #
  # See ./carrier-attachment.nix. THE NAME IS THE ADDRESS, and that is a
  # property of this carrier rather than a shortcut: a `100.x` is assigned by
  # the coordination server and is not knowable at eval time, which is the fact
  # that blocked declaring tailnet attachments at all. A MagicDNS name is
  # knowable from the host's own name, so the half needs no declared
  # `attachments.tailscale` -- which is also why tailscale is the one carrier
  # whose half is a pure function of the entity's name.
  #
  # The INTERFACE is derived for the same reason `gives` above gives -- but from
  # the HOST, not from a constant. This said "tailscaled always creates
  # `tailscale0`, so there is no per-host fact to state", and ../hosts/celler2.nix
  # is the counterexample: no TUN in a container, so no interface at all.
  den.carrierAttachments.tailscale =
    let
      nameOf = h: "${lib.toLower h.name}.${den.aspects.tailscale.domain}";
    in
    {
      # NO `attachesVia`: there is no `attachments.tailscale` and there should
      # not be. A `100.x` is assigned by the coordination server and is not
      # knowable at eval time, which is the fact that blocked declaring tailnet
      # attachments at all -- and the name that replaces it is a function of
      # the host's own name, so there is nothing for a host to write down.
      attachesVia = [ ];
      dialAt = nameOf;
      interfacesOn = h: [ (ifaceFor h) ];

      # MagicDNS answers the bare name inside the tailnet too, which is what
      # `tailscale ssh secondpc` and ../hosts/iphone.nix's `MagicDNSSuffix`
      # lookup both rely on.
      hostnameAt = nameOf;
      aliasesAt = h: [ (lib.toLower h.name) ];
    };

  den.aspects.tailscale.carrier-attachment =
    { host, ... }:
    [ (carrierAttachment.half "tailscale" host) ];
}
