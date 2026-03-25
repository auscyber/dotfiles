# Programme home-manager modules.
{ ... }:
{
  flake.modules.homeManager = {
    program-1password  = ../_home/programs/1password/default.nix;
    program-openclaw   = ../_home/programs/openclaw/default.nix;
    program-sketchybar = ../_home/programs/sketchybar/default.nix;
    program-zotero     = ../_home/programs/zotero/default.nix;
  };
}
