# nix-darwin-only feature modules (no NixOS/Home Manager counterpart).
# Cross-cutting features (general, builders, keybinds, secrets) are in their
# own top-level combined modules.
{ ... }:
{
  flake.modules.darwin = {
    darwin-finder              = ../_darwin/finder/default.nix;
    darwin-hmApps              = ../_darwin/hmApps/default.nix;
    darwin-homebrew            = ../_darwin/homebrew/default.nix;
    darwin-keys                = ../_darwin/keys/default.nix;
    darwin-network             = ../_darwin/network/default.nix;
    darwin-security-pam        = ../_darwin/security/pam/default.nix;
    darwin-security-sudoagents = ../_darwin/security/sudoagents/default.nix;
  };
}
