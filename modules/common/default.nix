# Cross-platform modules shared between NixOS, nix-darwin and home-manager.
# Each attribute registers the module under a named key in flake.modules so
# the system builders can pick them up via `builtins.attrValues`.
#
# NOTE: features that span multiple platform dirs now live in their own
# combined top-level modules (general.nix, nix-builds.nix, secrets-config.nix,
# ssh.nix, 1password.nix, keybinds.nix).  Only truly shared modules that have
# no platform-specific counterpart remain here.
{ ... }:
{
  flake.modules = {
    # ── NixOS ──────────────────────────────────────────────────────────────
    nixos = {
      common-allConfigs = ../_common/allConfigs.nix;
      common-common     = ../_common/common/default.nix;
      common-hm         = ../_common/hm/default.nix;
      common-nix        = ../_common/nix/default.nix;
      common-ssh-keys   = ../_common/ssh-keys.nix;
      common-vpn        = ../_common/vpn.nix;
    };

    # ── nix-darwin ─────────────────────────────────────────────────────────
    darwin = {
      common-allConfigs = ../_common/allConfigs.nix;
      common-common     = ../_common/common/default.nix;
      common-hm         = ../_common/hm/default.nix;
      common-nix        = ../_common/nix/default.nix;
      common-ssh-keys   = ../_common/ssh-keys.nix;
      common-vpn        = ../_common/vpn.nix;
    };

    # ── Home-Manager (shared modules for every HM evaluation) ──────────────
    homeManager = {
      common-allConfigs = ../_common/allConfigs.nix;
      common-nix        = ../_common/nix/default.nix;
    };
  };
}
