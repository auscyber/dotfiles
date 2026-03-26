# 1Password — system-level polkit integration on NixOS and nix-darwin lives in
# _common/1password; the shell-plugins / CLI integration lives in the
# Home Manager module.
{ ... }:
{
  flake.modules = {
    nixos.system-1password   = ../_common/1password/default.nix;
    darwin.system-1password  = ../_common/1password/default.nix;
    homeManager.hm-1password = ../_home/programs/1password/default.nix;
  };
}
