{
  den,
  lib,
  rootPath,
  fleet,
  ...
}:
# Remote build machines. In aspects/base rather than aspects/nixos because it
# declares both a `nixos` and a `darwin` class.
let
  # Every machine in the fleet (../hosts/fleet.nix) that advertises a `builder`
  # record.

  # `hostName` is what ssh dials. `ipAddress` is the older spelling and still
  # works; an external builder reached by DNS (faggot.sh) sets `hostName`.
  # ── WHERE SSH REACHES IT, PREFERRING A DECLARED ATTACHMENT ──
  #
  # `hostName`/`ipAddress` on the record is the fallback. An entry that says
  # WHICH carrier it is on -- `attachments.<carrier>.address`, the same key a
  # den host declares -- is stating a fact the registry can read, and that is
  # preferred: it is the half of this that will be derived outright once an
  # external is a stub entity and the carrier can furnish its own answer.
  #
  # ORDER IS DELIBERATE: `hostName` first, because a declared attachment is not
  # automatically ssh-able. Measured on faggot.sh -- its tailnet address answers
  # a TCP connect on 22 and refuses ssh, because the tailnet policy declines it.
  dialled =
    host:
    let
      b = host.builder or { };
      stated = b.hostName or b.ipAddress or "";
      attached = lib.filterAttrs (_: a: (a.address or null) != null) (host.attachments or { });
      firstAttached = if attached == { } then null else (lib.head (lib.attrValues attached)).address;
    in
    if stated != "" then
      stated
    else if firstAttached != null then
      firstAttached
    else
      "";

  # ── `publicHostKey` IS A DERIVED VALUE, SO DERIVE IT ──
  #
  # `nix.buildMachines.*.publicHostKey` wants base64 of the host key LINE, and
  # the entity already carries that line as `hostPublicKey` (what agenix rekeys
  # to, and what `hosts-holding-secrets-have-a-key` checks for). So the base64
  # in each `builder` record was the same key a second time, in a second
  # encoding, written by hand -- measured: faggot.sh's and auspc's recorded
  # values are byte-for-byte `toBase64 hostPublicKey`.
  #
  # LOCAL, because `lib.strings.toBase64` does not exist in this nixpkgs
  # (checked) and these two call sites are the only consumers. Verified against
  # both real keys and against RFC 4648's padding cases (`a` -> `YQ==`,
  # `ab` -> `YWI=`, `abc` -> `YWJj`, `""` -> `""`).
  toBase64 =
    str:
    let
      alphabet = lib.stringToCharacters "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
      bytes = map lib.strings.charToInt (lib.stringToCharacters str);
      n = builtins.length bytes;
      rem = n - (n / 3) * 3;
      pad = if rem == 0 then 0 else 3 - rem;
      padded = bytes ++ lib.replicate pad 0;
      groups = (builtins.length padded) / 3;
      enc = lib.concatMapStrings (
        i:
        let
          v =
            (lib.elemAt padded (i * 3)) * 65536
            + (lib.elemAt padded (i * 3 + 1)) * 256
            + (lib.elemAt padded (i * 3 + 2));
          sel = k: lib.elemAt alphabet (builtins.bitAnd (v / k) 63);
        in
        sel 262144 + sel 4096 + sel 64 + sel 1
      ) (lib.range 0 (groups - 1));
    in
    if n == 0 then
      ""
    else
      builtins.substring 0 (builtins.stringLength enc - pad) enc
      + lib.concatStrings (lib.replicate pad "=");

  # The recorded value if one is stated, otherwise the entity's own key in the
  # encoding nix wants. `hostPublicKey` can be declared-and-null, so it is
  # TESTED rather than `or`-ed (../../CLAUDE.md).
  hostKeyBase64 =
    host:
    let
      stated = host.builder.publicHostKey or null;
      line = host.hostPublicKey or null;
    in
    if stated != null then
      stated
    else if line == null then
      ""
    else
      # ── THE KEY MATERIAL, WITHOUT THE COMMENT ──
      # The two recorded values disagreed about this and both worked: auspc's
      # base64 included its ` root@nixos` comment and secondpc's did not
      # (measured by decoding both). A comment is legal in a known_hosts line
      # and is not part of the key, so the canonical two fields are what gets
      # encoded -- which makes every host's value derived the same way instead
      # of however it was pasted.
      toBase64 (lib.concatStringsSep " " (lib.take 2 (lib.splitString " " line)));

  # Every fleet member, den host or external, that advertises a builder.
  collectBuilders = lib.filterAttrs (
    _: host: (host.builder or null) != null && dialled host != ""
  ) fleet;

  entities = lib.concatMapAttrs (_: hs: hs) den.hosts;

  # The real entity for a class body's `host`, which carries den's PROJECTED
  # `hasAspect` -- see ./capability.nix's `bindAddressFor`.
  entityOf = host: entities.${host.name or ""} or host;

  # ── THE CREDENTIAL FOR ONE BUILD LINK, REALISED ──
  # `host`/`peer` are what the builder's `gives` is evaluated with: the builder
  # furnishing it and the client asking. `giverHost`/`takerHost` are the same
  # two, said for the credential, which keys its folder by them.
  linkCredential =
    client: builder:
    den.lib.capability.realise "builder" {
      giver = den.aspects.nix-builder;
      taker = den.aspects.builders;
      host = builder;
      peer = client;
      giverHost = builder;
      takerHost = client;
    };

  # The record a builder's `gives` is evaluated against: its den host where it
  # is one, its fleet entry otherwise. Either carries the `builder` record, and
  # that is all the give reads -- so an external's credential is realised the
  # same way, its `rekeyFile` simply stated rather than generated.
  builderRecord = name: entities.${name} or (fleet.${name} // { inherit name; });

  buildersFor =
    self:
    lib.mapAttrs (n: _: builderRecord n) (
      lib.filterAttrs (n: _: n != (self.name or "")) collectBuilders
    );

  # Hosts that can offload a build: the classes `builders` configures below.
  # Everything else -- a phone, an iOS device -- has no nix daemon to present a
  # key, so minting it a link would be a key nothing could use.
  clientsFor =
    self:
    lib.filterAttrs (
      n: h:
      n != (self.name or "")
      && lib.elem (h.class or null) [
        "nixos"
        "darwin"
      ]
    ) entities;

  # ── THE KEY A CLIENT PRESENTS TO ONE BUILDER ──
  # That link's own credential, read through `scoped`.
  keyPathFor =
    scoped: self: name:
    (linkCredential self (builderRecord name)).taker.pathIn scoped;

  mkBuildMachine = scoped: self: name: host: {
    protocol = "ssh-ng";
    hostName = dialled host;
    systems = host.builder.systems or [ ];
    publicHostKey = hostKeyBase64 host;
    maxJobs = host.builder.maxJobs or 1;
    speedFactor = host.builder.speedFactor or 1;
    supportedFeatures = host.builder.features or [ ];
    sshUser = host.builder.sshUser or "builder";
    # The build client's own key, not the operator's. `nix.buildMachines` is
    # consumed by the nix *daemon*, which runs as root and would otherwise
    # reach for root's default identity -- a key that exists on no host here.
    #
    # PER BUILDER, so a client presents a different identity to each one.
    sshKey = keyPathFor scoped self name;
  };

  # Minus the host asking, which the old `den.hosts` walk could not express:
  # `collectBuilders` was computed at the top level with no host in scope, so a
  # builder that also *used* builders (secondpc) listed itself and offered to
  # offload its own builds to itself over ssh.
  buildMachinesFor =
    self: scoped:
    lib.mapAttrsToList (mkBuildMachine scoped self) (
      lib.filterAttrs (n: _: n != (self.name or "")) collectBuilders
    );

  builderOptions = ''
    builders-use-substitutes = true
  '';

  # Shared by the `nixos` and `darwin` classes below, which are separate classes
  # and so cannot share a `let` binding of their own.
  clientConfig =
    {
      host,
      scoped,
      ...
    }:
    let
      self = entityOf host;
    in
    {
      nix.distributedBuilds = true;
      nix.extraOptions = builderOptions;
      nix.buildMachines = buildMachinesFor self scoped;

      # ── THIS CLIENT'S HALF OF EVERY LINK ──
      # Installed the way ../services/postgres.nix installs its own: the
      # realisation defines the entries, this only places them.
      age.scoped = lib.mkMerge (
        lib.mapAttrsToList (
          _: builder:
          let
            r = linkCredential self builder;
          in
          {
            ${r.scope}.secrets = r.takerSecrets;
          }
        ) (buildersFor self)
      );
    };
in
{
  # The build client's SSH key, as a generated agenix secret.
  #
  # `ssh-keygen` writes the private key to stdout (which agenix encrypts) and
  # the public half to a `.pub` sidecar next to the secret, exactly as
  # ../network/vpn.nix's `wireguard_pair` does for wireguard -- that sidecar is
  # the whole point, since a public key nobody can read is a public key nobody
  # can authorise.
  den.aspects.agenix-rekey.age.generators.ssh_ed25519_pair =
    {
      lib,
      file,
      pkgs,
      ...
    }:
    ''
      dir=$(mktemp -d)
      trap 'rm -rf "$dir"' EXIT
      ${pkgs.openssh}/bin/ssh-keygen -q -t ed25519 -N "" -C "nix-remote-build" -f "$dir/key"
      cat "$dir/key.pub" > ${lib.escapeShellArg (lib.removeSuffix ".age" file + ".pub")}
      cat "$dir/key"
    '';

  # Every build link's key is its credential's, realised per link (see
  # `linkCredential`), so this only brings in the machinery that decrypts them.
  den.aspects.builder-ssh-key = {
    includes = [ den.aspects.agenix-rekey ];
  };

  # `builders` capability. Populates `nix.buildMachines` from every fleet
  # entry carrying a `.builder` record.
  # ── A BUILDER IS A CAPABILITY, AND `systems` IS THE POINT OF IT ──
  #
  # The fold below reads a `.builder` record straight off the fleet registry.
  # That works and it is untyped: a misspelled `speedFactor` reaches
  # `nix.buildMachines` as nothing at all, nothing records WHICH host offered a
  # builder, and a consumer cannot ask for the one thing that actually
  # distinguishes them -- what it can build. `systems` is not a detail of a
  # builder, it is the whole selection criterion.
  #
  # Typed here, so a take can say "an aarch64-darwin builder" and resolution
  # answers with the ones that can, instead of the caller filtering a list it
  # had to know the shape of.
  den.capabilities.builder = {
    meta.description = "A machine that builds derivations for other hosts, and the systems it can build.";
    # ── A NARROWING OF `tcp`, SO THE ADDRESS IS RESOLVED AND NOT STATED ──
    # A builder is reached by ssh, which is a tcp endpoint like any other. So
    # it furnishes the PORT and resolution composes the address from the
    # nearest carrier the two ends share -- the same correction every other
    # consumer in this tree has had. `hostName`/`ipAddress` in the record was
    # one address for every asker: auspc's `10.100.0.2` sent secondpc's builds
    # through the wireguard tunnel to a machine on its own LAN.
    # ── NOT A SOCKET OF ITS OWN, WHICH IS THE CORRECTION HERE ──
    #
    # This narrowed `ssh` (and before that `tcp`) so that `dial` would supply
    # its address. Both were wrong, and `socket-exclusions` said so
    # immediately: `nix-builder` and `openssh` each claimed 127.0.0.1:22 on
    # auspc and on secondpc, because there is ONE sshd and two aspects were
    # describing it.
    #
    # A builder is not an endpoint. It is a FACT ABOUT A MACHINE that happens
    # to be reached by ssh -- which systems it can build, how many jobs it will
    # run, what features it offers. ../security/openssh.nix owns the socket,
    # the account and the host key; this owns the build facts. A consumer asks
    # two questions, because they are two questions: "which machine can build
    # x86_64-linux" and "how do I reach that machine".
    # ── AND IT CARRIES A CREDENTIAL, WHICH IS WHAT MAKES THE EDGE WORK ──
    #
    # Giving a builder entails a shared keypair: the client's nix daemon
    # presents it, the builder's `builder` account authorises it. That is
    # exactly `credential` -- "a secret both ends of an edge read, owned by
    # neither" -- and it is already true in implementation, just undeclared:
    # `age.generators.ssh_ed25519_pair` mints the pair, `secrets.builder-key`
    # rekeys the private half to every client, and the server side reads
    # `secrets/builder-key.pub` into the account's `authorizedKeys`.
    #
    # Declared here, so the two halves are one fact with one name instead of a
    # generator in one place and a `readFile` in another.
    includes = [
      den.capabilities.carried
      den.capabilities.credential
    ];

    options = {
      systems = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "Systems this machine can build for. What a consumer selects on.";
      };

      publicHostKey = lib.mkOption {
        type = lib.types.str;
        description = "Base64 of its host key line, the encoding `nix.buildMachines` wants.";
      };

      maxJobs = lib.mkOption {
        type = lib.types.int;
        default = 1;
        description = "How many builds it will run at once.";
      };

      speedFactor = lib.mkOption {
        type = lib.types.int;
        default = 1;
        description = "Relative speed, for nix's own scheduling. A rating against the others, not a measurement.";
      };

      # THE ACCOUNT TO OFFLOAD AS, which is not necessarily the account a
      # person lands in -- `ssh`'s `user` is that. nix connects as a dedicated
      # `builder` so a person's key and the daemon's are separate.
      sshUser = lib.mkOption {
        type = lib.types.str;
        default = "builder";
        description = "The account the nix daemon connects as.";
      };

      features = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        example = [ "big-parallel" ];
        description = "What it can do beyond building -- `big-parallel`, `kvm`, `nixos-test`. A derivation requiring one only goes to a machine offering it.";
      };
    };

    # ── IS IT ACTUALLY THERE, WHICH FOR A BUILDER IS A TCP CONNECT ──
    #
    # A builder is reached by ssh, so the test is whether ssh answers -- not
    # whether the box answers. Measured today on faggot.sh, which is a builder
    # in this very registry: ICMP replied 2/2 at 76ms while TCP 22 was
    # refused, so a ping check would have called it healthy while every build
    # offloaded to it failed. The port is the test.
    #
    # `nc -z` AND NOT AN `ssh`: a connect says whether the service is
    # listening, costs no authentication, and cannot trip the ban that refusing
    # authentication can -- which is how that port came to be refused.
    # NO PROBE OF ITS OWN: `ssh`'s is the right test and this inherits it
    # through the narrowing. A builder that answers ssh is a builder.

    taker.options.systems = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "x86_64-linux" ];
      description = "Systems the consumer needs built. Empty accepts any builder.";
    };
  };

  # ── GIVEN BY WHICHEVER HOST HAS THE RECORD, WITH NO PER-HOST EDIT ──
  #
  # Reads the entity's own `builder` record, so adding one to ../hosts/fleet.nix
  # is still the only edit -- the same property the fold below has, now typed.
  #
  # ⚠ ENTITIES ONLY, WHICH IS THE LIMIT OF THIS. `../hosts/fleet.nix`'s
  # `fleet.external."faggot.sh"` carries a `builder` record and is NOT an
  # entity, so it cannot give: `providersFor` asks `host.hasAspect` and an
  # external has no aspects. The fold below still reads it off the registry,
  # which is why both paths exist for now. A stub entity -- one declared for its
  # gives and takes and never instantiated -- is what would close that, and den
  # already models instantiation as a declared edge rather than a property of
  # being a host.
  den.aspects.nix-builder = {
    description = "Advertises this host as a remote builder, from its own fleet record.";

    gives =
      {
        self,
        host ? { },
        ...
      }:
      let
        b = host.builder or null;
      in
      lib.optionalAttrs (b != null && (b.hostName or b.ipAddress or "") != "") {
        # THE ACCOUNT THAT OWNS THE BUILDER'S COPY, which is the one this
        # aspect creates below -- not root, and not a person.
        user = _: {
          name = b.sshUser or "builder";
          group = b.sshUser or "builder";
          carrier = self.name;
        };

        builder =
          # `{ peer, ... }` AND NOT `_:`, which is the whole reason this can be
          # per link: the inner function is the per-pair one (../network/segments.nix
          # takes its `peer` the same way), so the asker is in scope here and
          # nowhere else.
          {
            peer ? { },
            ...
          }:
          {
            name = self.name;

            # NO PORT AND NO ADDRESS: both are ../security/openssh.nix's. Where
            # this machine is reached is the answer to `dial "ssh"` against it,
            # which is also what keeps the two aspects from claiming one socket.

            # ── WHICH CREDENTIAL, AND ITS GROUP ──
            # A stable name, as ../services/postgres.nix's `db-password` is.
            #
            # THE GROUP IS PER LINK, because both ends of a build link install
            # into it and neither end has only one: a client takes several
            # builders, a builder serves several clients, and secondpc is both.
            # One shared group would install `builder-key` several times on one
            # host. This give sees `peer`, so it can name the link itself.
            scope = "builder-${peer.name or "unknown"}-${host.name or "unknown"}";
            secretName = "builder-key";
            generator = "ssh_ed25519_pair";
            # A KEYPAIR: the builder authorises the `.pub`, it never needs the
            # client's private half.
            giverReads = "public";

            systems = b.systems or [ ];
            publicHostKey = hostKeyBase64 host;
            sshUser = b.sshUser or "builder";
            maxJobs = b.maxJobs or 1;
            speedFactor = b.speedFactor or 1;
            features = b.features or [ ];
          }
          # ── A STATED SOURCE ONLY WHERE THERE IS ONE ──
          # Furnishing `rekeyFile = null` would be a DEFINITION and would beat
          # the default, so it is absent unless the record names one. The
          # record is repo-relative because ../hosts/fleet.nix has no root to
          # make a path from; this file does.
          // lib.optionalAttrs ((b.rekeyFile or null) != null) {
            rekeyFile = rootPath + "/${b.rekeyFile}";
          };
      };

    # ── IT *USES* THIS HOST'S SSHD, SAID AS A TAKE ──
    # Not a second giver of the same socket. ../security/openssh.nix owns port
    # 22, the account and the host key; this says it depends on them. So the
    # address a client offloads to is the answer to resolving `ssh` against the
    # builder's host, and `socket-exclusions` has nothing to complain about --
    # it reported this exact pair (`nix-builder`'s and `openssh`'s 127.0.0.1:22
    # on auspc and secondpc) when this was modelled as a narrowing.
    takes.ssh = { };

    # ANY host may offload to it, so the whole fleet.
    reach = "fleet";
  };

  den.aspects.builders = {
    includes = [ den.aspects.builder-ssh-key ];

    # ── WHO READS THE CLIENT'S COPY: THE DAEMON, SO ROOT ──
    # `credential`'s realisation resolves each side's `user` to decide who owns
    # that side's secret, and refuses without one -- measured: "the taker
    # 'builders' of a credential gives no `user`, so there is no account to own
    # its copy". `nix.buildMachines` is consumed by the nix DAEMON, which runs
    # as root, which is also why the key cannot live in a person's homedir.
    gives =
      { self, ... }:
      {
        user = _: {
          name = "root";
          group = "root";
          carrier = self.name;
        };
      };

    takes.builder = { };

    nixos = clientConfig;
    darwin = clientConfig;
  };

  # Every host is a build client. Declared here rather than per-host so giving a
  # host a `builder` record is the only edit needed to give the whole fleet
  # access to it.
  #
  # Note that the two Linux builders are only reachable over the wireguard
  # tunnel (10.100.0.x). A host that is off the tunnel will fail to reach them
  # and nix will fall back to building locally, which costs a connection timeout
  # per build rather than an error; faggot.sh is dialled by DNS and has no such
  # constraint.
  den.schema.host.includes = [
    den.aspects.builders
    den.aspects.nix-builder
  ];

  # `builder-server` — include on a host that advertises itself as a builder
  # and accepts builds from other hosts. Sets up the `builder` SSH user with
  # the master key authorised.
  den.aspects.builder-server = {
    nixos =
      {
        host,
        pkgs,
        config,
        ...
      }:
      let
        b = host.builder or null;
        self = entityOf host;

        # ── THIS BUILDER'S HALF OF EVERY LINK ──
        links = lib.mapAttrsToList (_: client: linkCredential client self) (clientsFor self);

        # The public half `ssh_ed25519_pair` writes beside the source, which is
        # the `rekeyFile` the giver's copy inherits -- read back off the
        # declared secret rather than composed, so it cannot name another file.
        # `pathExists` because nothing is minted on a fresh checkout, and
        # reading a missing file would take down every evaluation, including
        # the one that runs the generator.
        pubFor =
          r:
          let
            src = toString config.age.secrets."${r.scope}/${r.taker.key}".rekeyFile;
            pub = lib.removeSuffix ".age" src + ".pub";
          in
          lib.optional (builtins.pathExists pub) (lib.removeSuffix "\n" (builtins.readFile pub));
      in
      lib.mkIf (b != null) {
        age.scoped = lib.mkMerge (map (r: { ${r.scope}.secrets = r.giverSecrets; }) links);

        services.openssh.extraConfig = ''
          SetEnv PATH=/nix/var/nix/profiles/default/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
        '';
        nix.settings.trusted-users = [ b.sshUser ];
        users.users.${b.sshUser} = {
          isSystemUser = true;
          group = b.sshUser;
          shell = pkgs.bashInteractive;
          openssh.authorizedKeys.keys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILeCdR16VYTNmoEekYk/b1sskC+trPx9tpOBJoKML17H"
          ]
          # ── ONE KEY PER LINK THAT TERMINATES HERE ──
          # Each read from the `.pub` beside that link's secret.
          ++ lib.concatMap pubFor links;
        };
        users.groups.${b.sshUser} = { };
      };

    darwin =
      {
        host,
        pkgs,
        ...
      }:
      let
        b = host.builder or null;
      in
      lib.mkIf (b != null) {
        nix.settings.trusted-users = [ b.sshUser ];
        users.knownUsers = [ b.sshUser ];
        users.users.${b.sshUser} = {
          uid = 3000;
          shell = pkgs.bashInteractive;
        };
      };
  };
}
