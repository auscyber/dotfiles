# Shell home-manager modules.
{ ... }:
{
  flake.modules.homeManager = {
    shell      = ../_home/shell/default.nix;
    shell-fish = ../_home/shell/fish/default.nix;
    shell-zsh  = ../_home/shell/zsh/default.nix;
  };
}
