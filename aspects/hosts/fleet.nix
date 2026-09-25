# The fleet as one attrset, for the aspects that need every machine at once
# (ssh known_hosts and aliases, the wireguard peer list, remote builders,
# tailscale enrolment). Built from `den.hosts` -- each host declares its own
# `hostPublicKey`, `builder` and `device` -- plus `fleet.external`, for machines
# we dial but do not build. Handed to modules as the `fleet` argument.
#
#   <name> = {
#     system;          den host only
#     hostPublicKey;   only when declared
#     user;            the login account peers SSH into: the host's user
#     uid;             that account's uid (501 on darwin, 1000 elsewhere)
#     builder;         `nix.buildMachines` record, or null
#     device;          tailscale enrolment record, or null
#   }
{
  den,
  lib,
  config,
  ...
}:
let
  inherit (lib) mkOption types;
  record = types.nullOr (types.attrsOf types.anything);

  fromHost =
    _: h:
    {
      inherit (h) system builder device;
      user = if h.users == { } then null else lib.head (builtins.attrNames h.users);
      uid = if lib.hasSuffix "darwin" h.system then 501 else 1000;
    }
    // lib.optionalAttrs h.hasHostPublicKey { inherit (h) hostPublicKey; };
in
{
  options.fleet.external = mkOption {
    type = types.attrsOf (types.attrsOf types.anything);
    default = { };
    description = "Machines in the fleet that are not den hosts, in the same shape as the rest of `fleet`.";
  };

  config = {
    den.schema.host =
      { options, ... }:
      {
        options = {
          builder = mkOption {
            type = record;
            default = null;
            description = ''
              Present iff the host is a remote build machine: `hostName` or
              `ipAddress`, `publicHostKey` (base64), `systems`, `maxJobs`,
              `speedFactor`, `features`, `sshUser`. ../base/builders.nix gives
              every other host a `nix.buildMachines` entry for it.
            '';
          };
          device = mkOption {
            type = record;
            default = null;
            description = "Present iff the host is a tailscale client with nothing built for it (`description`); ../network/tailscale.nix enrols these.";
          };
          # Read instead of `hostPublicKey` itself, whose `apply` warns when it
          # is unset: walking the fleet would warn once per keyless host.
          hasHostPublicKey = mkOption {
            type = types.bool;
            readOnly = true;
            internal = true;
            default = options.hostPublicKey.isDefined;
          };
        };
      };

    _module.args.fleet =
      lib.mapAttrs fromHost (lib.concatMapAttrs (_: hosts: hosts) den.hosts) // config.fleet.external;

    # Not one of ours: an external NixOS box we only ever dial. It is here
    # because `builder` is what ../base/builders.nix walks, and a build machine
    # every host can reach over plain DNS (rather than only from inside the
    # wireguard tunnel) is exactly what that list is for. No `system`/`uid`,
    # since nothing builds or forwards an agent to it -- and no wireguard
    # keypair, which keeps it out of the tunnel peer list and the ssh aliases.
    fleet.external."faggot.sh" = {
      user = "ivy";
      # ed25519, from `ssh-keyscan faggot.sh`. The box also still offers the
      # ssh-rsa key that is already in ~/.ssh/known_hosts; this is the modern one.
      hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMWQGIJW7xP4ayNXSF0FOMdvpoPb1abvBxftF6L8ajn3";
      device = null;
      builder = {
        # `hostName`, not `ipAddress`: this one is reached by DNS.
        hostName = "faggot.sh";
        # base64 of the `hostPublicKey` line above, which is the encoding
        # `nix.buildMachines.*.publicHostKey` wants.
        publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSU1XUUdJSlc3eFA0YXlOWFNGMEZPTWR2cG9QYjFhYnZCeGZ0RjZMOGFqbjM=";
        systems = [ "x86_64-linux" ];
        # 12 cores, but across the public internet rather than a LAN, so it is
        # deliberately rated below auspc (20) and above secondpc (5).
        maxJobs = 6;
        speedFactor = 8;
        # No /dev/kvm on the box, so no `kvm` and no `nixos-test`.
        features = [ "big-parallel" ];
        sshUser = "ivy";
      };
    };
  };
}
