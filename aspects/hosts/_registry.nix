# The fleet's identity, as plain data.
#
# Every host declaration in this directory lives in a flake-parts partition
# (see ../../partition-map.nix), and a partition can only see its OWN aspects
# plus base -- so `den.hosts` is scoped to whichever partition is doing the
# evaluating. A NixOS host's `den.hosts` never contains a darwin host and vice
# versa. Anything cross-cutting therefore cannot be derived by walking it:
# ../network/vpn.nix silently dropped every darwin peer that way, and
# ../network/ssh-config.nix had no way to reach another host's SSH host key at
# all.
#
# This file is the answer, and it works because it is not an aspect: it starts
# with `_`, so the import tree skips it (see flake.nix), and any file in any
# partition can `import` it directly. Base data, not module output.
#
# It is the SINGLE source of truth for these fields -- each host file reads its
# own `hostPublicKey` back out of here rather than repeating it, so the copy a
# peer trusts and the copy agenix-rekey encrypts to cannot drift apart.
#
# Fields:
#   system          the host's nixpkgs system, matching `den.hosts.<system>`.
#   hostPublicKey   the host's OpenSSH *host* key: what agenix-rekey encrypts
#                   that host's secrets to, and what its peers pin in
#                   `programs.ssh.knownHosts`. Omitted for a host that has
#                   never had one declared.
#   user            the login account peers SSH into.
#   uid             that account's uid, needed for its gpg-agent socket path
#                   when forwarding GPG over the tunnel (../network/ssh-config.nix).
#   builder         present iff the host advertises itself as a remote build
#                   machine. Read by ../nixos/builders.nix to build
#                   `nix.buildMachines` on every host that includes
#                   `den.aspects.builders` -- which is the other thing a
#                   `den.hosts` walk could not do across a partition boundary,
#                   and the reason a Mac could never pick up a Linux builder.
#
# `imflopet` is deliberately absent: it is a `den.homes` entry (a standalone
# home on someone else's machine), not a host of ours, and the key it declares
# is a home key rather than an SSH host key.
#
# A host only joins the wireguard tunnel once `agenix generate` has produced its
# keypair under secrets/generated/<name>/vpn-secrets/; ../network/_lib.nix still
# checks for that file, so listing a host here does not by itself make it a peer.
{
  # Not one of ours: an external NixOS box we only ever dial. It is here because
  # `builder` is what ../base/builders.nix walks, and a build machine every host
  # can reach over plain DNS (rather than only from inside the wireguard tunnel)
  # is exactly what that list is for. No `system`/`uid`, since nothing builds or
  # forwards an agent to it -- and no wireguard keypair, which is what keeps it
  # out of the tunnel peer list and out of the generated ssh aliases.
  "faggot.sh" = {
    user = "ivy";
    # ed25519, from `ssh-keyscan faggot.sh`. The box also still offers the
    # ssh-rsa key that is already in ~/.ssh/known_hosts; this is the modern one.
    hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMWQGIJW7xP4ayNXSF0FOMdvpoPb1abvBxftF6L8ajn3";

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

  auspc = {
    system = "x86_64-linux";
    hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFYM1mZ3fYfOjhyMhIiKbUOLYTQifG82P2NnGWHyIwHt root@nixos";
    user = "auscyber";
    uid = 1000;

    # auspc has 4 cores reserved for builds and is the fastest box.
    builder = {
      ipAddress = "10.100.0.2";
      publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUZZTTFtWjNmWWZPamh5TWhJaUtiVU9MWVRRaWZHODJQMk5uR1dIeUl3SHQgcm9vdEBuaXhvcw==";
      systems = [ "x86_64-linux" ];
      maxJobs = 10;
      speedFactor = 20;
      features = [
        "big-parallel"
        "cached-compilation"
        "kvm"
      ];
      sshUser = "builder";
    };
  };

  secondpc = {
    system = "x86_64-linux";
    hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICj7wlOxTp0NQJoUhRtj7k8gtDC0lCr5MJqLV5LxG9Yf root@kexec-minimal";
    user = "auscyber";
    uid = 1000;

    builder = {
      ipAddress = "10.100.0.1";
      publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUNqN3dsT3hUcDBOUUpvVWhSdGo3azhndERDMGxDcjVNSnFMVjVMeEc5WWY=";
      systems = [ "x86_64-linux" ];
      maxJobs = 5;
      speedFactor = 5;
      features = [
        "big-parallel"
        "cached-compilation"
        "kvm"
      ];
      sshUser = "builder";
    };
  };

  # No `hostPublicKey`: surfacelaptop declares one on its *user*
  # (`den.hosts.…surfacelaptop.users.auscyber`) and never at host level, so
  # there is no host key to pin. The two are different credentials -- see
  # ../security/pubKey.nix -- and putting a user key in `knownHosts` would
  # pin the wrong one.
  surfacelaptop = {
    system = "x86_64-linux";
    user = "auscyber";
    uid = 1000;
  };

  Ivys-MacBook-Pro = {
    system = "aarch64-darwin";
    hostPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICTsjq9lMzer6RPeDfXZ9eI1eiMf8b/fteSOb5XC5rBG";
    user = "ivypierlot";
    # macOS starts its first human account at 501, not 1000.
    uid = 501;
  };

  # Likewise no host key has ever been declared here. Listed anyway: leaving it
  # out would make this file quietly incomplete, and `hostPublicKey`'s absence
  # is the fact worth recording.
  macmini = {
    system = "aarch64-darwin";
    user = "ivypierlot";
    uid = 501;
  };

  # --- devices ---
  #
  # Fleet members with a `device` attr and no `system`: phones and the like.
  # ../network/tailscale.nix builds its enrolment list from these. The other
  # consumers key off `builder` / `hostPublicKey` / a generated wireguard
  # keypair, so a device is inert in all of them.
  #
  # This is only the identity half. The phone IS a den host -- see
  # ../hosts/iphone.nix and ../framework/mobile.nix -- it just lands in
  # `mobileConfigurations` rather than `darwinConfigurations`.
  iphone = {
    user = "ivypierlot";
    device.description = "iPhone (Tailscale app)";
  };
}
