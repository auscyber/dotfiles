{
  den,
  lib,
  ...
}:
let
  # Walk every host registered under any system, collecting those that advertise
  # a non-null `builder` record with a non-empty ipAddress. Computed once at the
  # top level so it's stable across aspect evaluation contexts.
  collectBuilders =
    let
      allBySystem = den.hosts or { };
      keepBuilders =
        hosts:
        lib.filterAttrs (_: host:
          host ? builder
          && host.builder != null
          && (host.builder.ipAddress or "") != ""
        ) hosts;
    in
    lib.foldl' lib.recursiveUpdate { } (
      lib.attrValues (lib.mapAttrs (_: keepBuilders) allBySystem)
    );

  mkBuildMachine = host: {
    protocol = "ssh-ng";
    hostName = host.builder.ipAddress;
    systems = host.builder.systems or [ ];
    publicHostKey = host.builder.publicHostKey or "";
    maxJobs = host.builder.maxJobs or 1;
    speedFactor = host.builder.speedFactor or 1;
    supportedFeatures = host.builder.features or [ ];
    sshUser = host.builder.sshUser or "builder";
  };

  buildMachines = lib.mapAttrsToList (_: mkBuildMachine) collectBuilders;

  builderOptions = ''
    builders-use-substitutes = true
  '';
in
{
  # `builders` capability — include on a host that wants to USE builders.
  # Populates `nix.buildMachines` from every host's `.builder` record.
  den.aspects.builders = {
    nixos = {
      nix.distributedBuilds = true;
      nix.extraOptions = builderOptions;
      nix.buildMachines = buildMachines;
    };

    darwin = {
      nix.distributedBuilds = true;
      nix.extraOptions = builderOptions;
      nix.buildMachines = buildMachines;
    };
  };

  # `builder-server` — include on a host that advertises itself as a builder
  # and accepts builds from other hosts. Sets up the `builder` SSH user with
  # the master key authorised.
  den.aspects.builder-server = {
    nixos =
      { host, pkgs, ... }:
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
          ];
        };
        users.groups.${b.sshUser} = { };
      };

    darwin =
      { host, pkgs, ... }:
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
