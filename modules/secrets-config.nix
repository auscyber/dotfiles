# Secrets storage configuration — a single platform-guarded implementation in
# _common/secrets-platform covers NixOS and nix-darwin.
# The option definitions live in _common/secrets.nix.
# The Home Manager platform implementation lives in _home/secrets.
{ ... }:
{
  flake.modules = {
    # ── NixOS + nix-darwin: option definitions + platform storage ───────────
    generic = {
      secrets  = ../_common/secrets.nix;
      secrets-platform = ../_common/secrets-platform/default.nix;
    };
    # ── Home Manager: option definitions + HM-specific storage ──────────────
    homeManager = {
      secrets  = ../_common/secrets.nix;
      secrets-platform = ../_home/secrets/default.nix;
    };
  };
}
