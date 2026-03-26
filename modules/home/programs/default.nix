# Programme home-manager modules (home-only programs).
# The 1Password CLI/shell-plugins integration is in modules/1password.nix
# alongside the system-level 1Password polkit integration.
{ ... }:
{
  flake.modules.homeManager = {
    openclaw   = ../_home/programs/openclaw/default.nix;
    sketchybar = ../_home/programs/sketchybar/default.nix;
    zotero     = ../_home/programs/zotero/default.nix;
  };
}
