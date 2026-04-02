{ inputs, lib, ... }:
{
  # Import den's dendritic flake module which wires up den's own module tree
  # into the flake-parts evaluation and exposes den.hosts, den.homes,
  # den.aspects, den.schema, den.default, etc. as flake-parts options.
  imports = [
    (inputs.den.flakeModules.dendritic or { })
  ];

  # ── NixOS hosts ────────────────────────────────────────────────────────────
  den.hosts.x86_64-linux.auspc.users.auscyber = { };
  den.hosts.x86_64-linux.pentestvm.users.auscyber = { };
  den.hosts.x86_64-linux.secondpc.users.auscyber = { };
  den.hosts.x86_64-linux.surfacelaptop.users.auscyber = { };
  den.hosts.x86_64-linux.wsl-nixos.users.auscyber = { };

  # ── Darwin hosts ───────────────────────────────────────────────────────────
  den.hosts.aarch64-darwin."Ivys-MacBook-Pro".users.ivypierlot = { };
  den.hosts.aarch64-darwin.macmini.users.ivypierlot = { };

  # ── Standalone home configurations ─────────────────────────────────────────
  # These are home-manager configs for machines NOT in systems/ (e.g. managed
  # by another distro, a remote server, or a shared machine).
  den.homes.x86_64-linux."auscyber@arch" = { };
  den.homes.x86_64-linux."auscyber@laptop" = { };
  den.homes.x86_64-linux."ivy@imflopet" = { };
  den.homes.x86_64-linux."ivy@vmi1472413.contaboserver.net" = { };

  # Build standalone homeConfigurations for all users on all hosts.
  den.schema.user.classes = lib.mkDefault [ "homeManager" ];
}
