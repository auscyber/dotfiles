# Editor home-manager modules.
{ ... }:
{
  flake.modules.homeManager = {
    editor-neovim  = ../_home/editors/neovim/default.nix;
    editor-zed     = ../_home/editors/zed/default.nix;
    editor-emacs   = ../_home/emacs.nix;
    editor-kakoune = ../_home/kakoune.nix;
    editor-idris2  = ../_home/idris2.nix;
  };
}
