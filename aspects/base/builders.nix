{
  den,
  lib,
  rootPath,
  ...
}:
# Remote build machines.
#
# In aspects/base rather than aspects/nixos, even though the aspect names
# NixOS-shaped options: ../../partition-map.nix claims the whole `nixos`
# directory for the nixos bucket, so while this lived there `den.aspects
# .builders` did not exist in the darwin partition at all and the Mac could not
# include it -- which is what the commented-out line in ../hosts/laptop.nix was
# working around. It declares both a `nixos` and a `darwin` class and reaches
# for no NixOS-only input, so base is where it belongs.
let
  # Every machine that advertises a `builder` record.
  #
  # From ../hosts/_registry.nix, NOT `den.hosts`. `den.hosts` is scoped to the
  # partition doing the evaluating, so walking it here meant a darwin host's
  # `nix.buildMachines` could only ever contain darwin builders -- of which
  # there are none. The registry is a plain data file every partition can
  # import, so the same walk now sees the whole fleet.
  registry = import ../hosts/_registry.nix;

  # `hostName` is what ssh dials. `ipAddress` is the older spelling and still
  # works; an external builder reached by DNS (faggot.sh) sets `hostName`.
  dialled = b: b.hostName or b.ipAddress or "";

  collectBuilders = lib.filterAttrs (
    _: host: host ? builder && host.builder != null && dialled host.builder != ""
  ) registry;

  mkBuildMachine = sshKey: host: {
    protocol = "ssh-ng";
    hostName = dialled host.builder;
    systems = host.builder.systems or [ ];
    publicHostKey = host.builder.publicHostKey or "";
    maxJobs = host.builder.maxJobs or 1;
    speedFactor = host.builder.speedFactor or 1;
    supportedFeatures = host.builder.features or [ ];
    sshUser = host.builder.sshUser or "builder";
    # The build client's own key, not the operator's. `nix.buildMachines` is
    # consumed by the nix *daemon*, which runs as root and would otherwise
    # reach for root's default identity -- a key that exists on no host here.
    inherit sshKey;
  };

  # Minus the host asking, which the old `den.hosts` walk could not express:
  # `collectBuilders` was computed at the top level with no host in scope, so a
  # builder that also *used* builders (secondpc) listed itself and offered to
  # offload its own builds to itself over ssh.
  buildMachinesFor =
    name: sshKey:
    lib.mapAttrsToList (_: mkBuildMachine sshKey) (lib.filterAttrs (n: _: n != name) collectBuilders);

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
    {
      nix.distributedBuilds = true;
      nix.extraOptions = builderOptions;
      nix.buildMachines = buildMachinesFor host.hostName scoped.builder-ssh-key.secrets.fargonekey.path;
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

  den.aspects.builder-ssh-key = {
    includes = [ den.aspects.agenix-rekey ];

    # ONE key for the whole fleet, not one per host. An explicit `rekeyFile` is
    # what makes that so: without it agenix-rekey derives the source path from
    # `age.rekey.generatedSecretsDir`, which is per-host
    # (secrets/generated/<host>/), and `agenix generate` would mint a separate
    # keypair for every machine -- five public keys to paste into one server's
    # authorized_keys instead of one.
    #
    # `rootPath + "/secrets/..."`, not `./fargonekey.age`: a Nix path literal is
    # copied into the store the moment it is interpolated, so the generator
    # would be asked to write into a read-only store path -- and the file does
    # not exist yet, which is the point. The string form keeps it a real,
    # writable path in the working tree. Same shape ../dev/nix-dev.nix uses for
    # its github token.
    secrets.fargonekey = {
      rekeyFile = rootPath + "/secrets/fargonekey.age";
      generator.script = "ssh_ed25519_pair";
    };
  };

  # `builders` capability. Populates `nix.buildMachines` from every registry
  # entry carrying a `.builder` record.
  den.aspects.builders = {
    includes = [ den.aspects.builder-ssh-key ];
    nixos = clientConfig;
    darwin = clientConfig;
  };

  # Every host is a build client. Declared here rather than per-host so adding a
  # builder to the registry is the only edit needed to give the whole fleet
  # access to it.
  #
  # Note that the two Linux builders are only reachable over the wireguard
  # tunnel (10.100.0.x). A host that is off the tunnel will fail to reach them
  # and nix will fall back to building locally, which costs a connection timeout
  # per build rather than an error; faggot.sh is dialled by DNS and has no such
  # constraint.
  den.schema.host.includes = [ den.aspects.builders ];

  # `builder-server` — include on a host that advertises itself as a builder
  # and accepts builds from other hosts. Sets up the `builder` SSH user with
  # the master key authorised.
  den.aspects.builder-server = {
    nixos =
      {
        host,
        pkgs,
        ...
      }:
      let
        b = host.builder or null;
      in
      lib.mkIf (b != null) {
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
          # The fleet's own build-client key (above), once it has been
          # generated. `pathExists` because it has not been on a fresh checkout,
          # and reading a missing file would take down every evaluation --
          # including the one that runs the generator meant to create it.
          ++ lib.optional (builtins.pathExists (rootPath + "/secrets/fargonekey.pub")) (
            lib.removeSuffix "\n" (builtins.readFile (rootPath + "/secrets/fargonekey.pub"))
          );
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
