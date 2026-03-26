# nix-darwin-only feature modules (no NixOS/Home Manager counterpart).
# Cross-cutting features (general, builders, keybinds, secrets) are in their
# own top-level combined modules.
{ ... }:
{
  flake.modules.darwin = {
    finder              = ../_darwin/finder/default.nix;
    hmApps              = ../_darwin/hmApps/default.nix;
    homebrew            = ../_darwin/homebrew/default.nix;
    keys                = ../_darwin/keys/default.nix;
    network             = ../_darwin/network/default.nix;
    security-pam        = ../_darwin/security/pam/default.nix;
    security-sudoagents = ../_darwin/security/sudoagents/default.nix;
  };
}
