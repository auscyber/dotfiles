# Shell home-manager modules.
{ ... }:
{
  flake.modules.homeManager = {
    shell      = ../_home/shell/default.nix;
    fish = ../_home/shell/fish/default.nix;
    zsh  = ../_home/shell/zsh/default.nix;
  };
}
