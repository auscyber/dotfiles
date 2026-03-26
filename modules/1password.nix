# 1Password — system-level polkit integration on NixOS and nix-darwin lives in
# _common/1password; the shell-plugins / CLI integration lives in the
# Home Manager module.
# The system module is registered under generic so it applies to both OS types.
{ ... }:
{
  flake.modules = {
    generic.system-1password     = ../_common/1password/default.nix;
    homeManager.hm-1password     = ../_home/programs/1password/default.nix;
  };
}
