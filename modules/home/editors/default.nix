# Editor home-manager modules.
{ ... }:
{
  flake.modules.homeManager = {
    neovim  = ../_home/editors/neovim/default.nix;
    zed     = ../_home/editors/zed/default.nix;
    emacs   = ../_home/emacs.nix;
    kakoune = ../_home/kakoune.nix;
    idris2  = ../_home/idris2.nix;
  };
}
