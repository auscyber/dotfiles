# Cross-platform modules shared between NixOS, nix-darwin and home-manager.
# Each attribute registers the module under a named key in flake.modules so
# the system builders can pick them up via `builtins.attrValues`.
{ ... }:
{
  flake.modules = {
    # ── NixOS ──────────────────────────────────────────────────────────────
    nixos = {
      common-1password  = ../_common/1password/default.nix;
      common-allConfigs = ../_common/allConfigs.nix;
      common-builders   = ../_common/builders/default.nix;
      common-common     = ../_common/common/default.nix;
      common-hm         = ../_common/hm/default.nix;
      common-nix        = ../_common/nix/default.nix;
      common-secrets    = ../_common/secrets.nix;
      common-ssh-keys   = ../_common/ssh-keys.nix;
      common-vpn        = ../_common/vpn.nix;
    };

    # ── nix-darwin ─────────────────────────────────────────────────────────
    darwin = {
      common-1password  = ../_common/1password/default.nix;
      common-allConfigs = ../_common/allConfigs.nix;
      common-builders   = ../_common/builders/default.nix;
      common-common     = ../_common/common/default.nix;
      common-hm         = ../_common/hm/default.nix;
      common-kmonad     = ../_common/kmonad/default.nix;
      common-nix        = ../_common/nix/default.nix;
      common-secrets    = ../_common/secrets.nix;
      common-ssh-keys   = ../_common/ssh-keys.nix;
      common-vpn        = ../_common/vpn.nix;
    };

    # ── Home-Manager (shared modules for every HM evaluation) ──────────────
    homeManager = {
      common-allConfigs = ../_common/allConfigs.nix;
      common-nix        = ../_common/nix/default.nix;
      common-secrets    = ../_common/secrets.nix;
    };
  };
}
