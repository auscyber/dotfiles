# nix-darwin-specific feature modules.
{ ... }:
{
  flake.modules.darwin = {
    darwin-builders          = ../_darwin/builders/default.nix;
    darwin-finder            = ../_darwin/finder/default.nix;
    darwin-general           = ../_darwin/general/default.nix;
    darwin-hmApps            = ../_darwin/hmApps/default.nix;
    darwin-homebrew          = ../_darwin/homebrew/default.nix;
    darwin-karabiner-driver  = ../_darwin/keybinds/karabiner_driver/default.nix;
    darwin-keys              = ../_darwin/keys/default.nix;
    darwin-network           = ../_darwin/network/default.nix;
    darwin-security-pam      = ../_darwin/security/pam/default.nix;
    darwin-security-secrets  = ../_darwin/security/secrets/default.nix;
    darwin-security-sudoagents = ../_darwin/security/sudoagents/default.nix;
  };
}
