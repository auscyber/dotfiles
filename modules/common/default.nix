# Cross-platform modules shared between NixOS and nix-darwin.
# Each attribute registers the module under a named key in flake.modules so
# the system builders can pick them up via `builtins.attrValues`.
#
# flake.modules.generic  — included in BOTH NixOS and nix-darwin evaluations.
# flake.modules.homeManager — included in every Home Manager evaluation.
#
# NOTE: features that span multiple platform dirs now live in their own
# combined top-level modules (general.nix, nix-builds.nix, secrets-config.nix,
# ssh.nix, 1password.nix, keybinds.nix).  Only truly shared modules that have
# no platform-specific counterpart remain here.
{ ... }:
{
  flake.modules = {
    # ── NixOS + nix-darwin (system-level) ─────────────────────────────────
    generic = {
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
