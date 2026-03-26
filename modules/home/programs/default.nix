# Programme home-manager modules (home-only programs).
# The 1Password CLI/shell-plugins integration is in modules/1password.nix
# alongside the system-level 1Password polkit integration.
{ ... }:
{
  flake.modules.homeManager = {
    program-openclaw   = ../_home/programs/openclaw/default.nix;
    program-sketchybar = ../_home/programs/sketchybar/default.nix;
    program-zotero     = ../_home/programs/zotero/default.nix;
  };
}
